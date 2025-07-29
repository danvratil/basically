// SPDX-FileCopyrightText: 2025 Daniel Vrátil <me@dvratil.cz>
//
// SPDX-License-Identifier: MIT

use std::collections::VecDeque;

use pest::iterators::Pair;
use thiserror::Error;

use crate::parser::Rule;

#[derive(Error, Debug)]
pub enum AstError {
    #[error("Empty program")]
    EmptyProgram,

    #[error("Invalid statement: {0}")]
    InvalidStatement(String),

    #[error("Invalid expression: {0}")]
    InvalidExpression(String),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl TryFrom<Pair<'_, Rule>> for Program {
    type Error = AstError;

    fn try_from(program: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match program.as_rule() {
            Rule::program => Ok(Program {
                statements: program
                    .into_inner()
                    .take_while(|p| p.as_rule() != Rule::EOI)
                    .map(Statement::try_from)
                    .collect::<Result<Vec<_>, AstError>>()?,
            }),
            _ => Err(AstError::EmptyProgram),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ArrayDimension {
    pub lower: Option<Expression>,
    pub upper: Expression,
}

#[derive(Debug, Clone)]
pub struct ArrayDeclaration {
    pub name: String,
    pub element_type: VariableType,
    pub dimensions: Vec<ArrayDimension>,
}

#[derive(Debug, Clone)]
pub struct ArrayAccess {
    pub name: String,
    pub indices: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum AssignmentTarget {
    Variable(Variable),
    ArrayElement(ArrayAccess),
}

#[derive(Debug, Clone)]
pub struct IfBranch {
    pub condition: Option<Expression>, // None for ELSE branch
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum DoConditionType {
    PreTestWhile,  // DO WHILE condition
    PreTestUntil,  // DO UNTIL condition
    PostTestWhile, // LOOP WHILE condition
    PostTestUntil, // LOOP UNTIL condition
    None,          // DO...LOOP (infinite) - implemented but not tested initially
}

#[derive(Debug, Clone)]
pub enum Statement {
    NumberedStatement { line_number: u32, statement: PlainStatement },
    PlainStatement(PlainStatement),
}

#[derive(Debug, Clone)]
pub enum PlainStatement {
    Noop,
    Print(Expression),
    Assignment {
        target: AssignmentTarget,
        expression: Expression,
    },
    Input {
        prompt: Option<String>,
        variables: Vec<Variable>,
    },
    Metacommand(Metacommand),
    Dim(ArrayDeclaration),
    If {
        branches: Vec<IfBranch>, // First is IF, middle are ELSEIF, last might be ELSE
    },
    For {
        counter: Variable,
        start: Expression,
        end: Expression,
        step: Option<Expression>, // None means step = 1
        statements: Vec<Statement>,
    },
    DoLoop {
        condition_type: DoConditionType,
        condition: Option<Expression>, // None for infinite loop
        statements: Vec<Statement>,
    },
    While {
        condition: Expression,
        statements: Vec<Statement>,
    },
    ExitDo,
    ExitFor,
    Goto(GotoTarget),
    Label(String),
}

#[derive(Debug, Clone)]
pub enum GotoTarget {
    LineNumber(u32),
    Label(String),
}

impl Statement {
    /// Get the inner PlainStatement regardless of whether it's numbered
    pub fn inner_statement(&self) -> &PlainStatement {
        match self {
            Statement::NumberedStatement { statement, .. } => statement,
            Statement::PlainStatement(statement) => statement,
        }
    }
    
    /// Get the line number if this is a numbered statement
    pub fn line_number(&self) -> Option<u32> {
        match self {
            Statement::NumberedStatement { line_number, .. } => Some(*line_number),
            Statement::PlainStatement(_) => None,
        }
    }
}

impl TryFrom<Pair<'_, Rule>> for Statement {
    type Error = AstError;

    fn try_from(statement: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match statement.as_rule() {
            Rule::numbered_statement => {
                let mut elements = statement.into_inner();
                let line_number_pair = elements.next().ok_or(
                    AstError::InvalidStatement("Missing line number".to_string()),
                )?;
                let statement_pair = elements.next().ok_or(
                    AstError::InvalidStatement("Missing statement after line number".to_string()),
                )?;

                let line_number = line_number_pair.as_str().parse::<u32>().map_err(|_| {
                    AstError::InvalidStatement("Invalid line number".to_string())
                })?;

                let statement = PlainStatement::try_from(statement_pair)?;

                Ok(Statement::NumberedStatement { line_number, statement })
            }
            Rule::plain_statement => {
                let statement = PlainStatement::try_from(statement)?;
                Ok(Statement::PlainStatement(statement))
            }
            Rule::label_definition => {
                let mut elements = statement.into_inner();
                let identifier = elements.next().ok_or(
                    AstError::InvalidStatement("Missing label identifier".to_string()),
                )?;
                let label_name = identifier.as_str().to_string();
                Ok(Statement::PlainStatement(PlainStatement::Label(label_name)))
            }
            Rule::comment => {
                let plain_statement = PlainStatement::try_from(statement)?;
                Ok(Statement::PlainStatement(plain_statement))
            }
            _ => Err(AstError::InvalidStatement(format!(
                "Expected numbered_statement, plain_statement, label_definition, or comment, got {:?}",
                statement.as_rule()
            ))),
        }
    }
}

impl TryFrom<Pair<'_, Rule>> for PlainStatement {
    type Error = AstError;

    fn try_from(statement: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match statement.as_rule() {
            Rule::plain_statement => {
                Ok(PlainStatement::try_from(statement.into_inner().next().ok_or(
                    AstError::InvalidStatement("Empty plain statement".to_string()),
                )?)?)
            }
            Rule::print_statement => Ok(PlainStatement::Print(Expression::try_from(
                statement
                    .into_inner()
                    .next()
                    .ok_or(AstError::InvalidStatement(
                        "Empty print statement".to_string(),
                    ))?,
            )?)),
            Rule::assignment_statement => {
                let mut elements = statement.clone().into_inner().collect::<VecDeque<_>>();
                if elements.len() != 3 {
                    return Err(AstError::InvalidStatement(
                        "Invalid assignment statement".to_string(),
                    ));
                }

                let first_element = elements.pop_front().ok_or(AstError::InvalidStatement(
                    "Empty assignment target".to_string(),
                ))?;

                let target = match first_element.as_rule() {
                    Rule::variable => {
                        AssignmentTarget::Variable(Variable::try_from(first_element)?)
                    }
                    Rule::array_access => {
                        AssignmentTarget::ArrayElement(ArrayAccess::try_from(first_element)?)
                    }
                    _ => {
                        return Err(AstError::InvalidStatement(
                            "Invalid assignment target".to_string(),
                        ));
                    }
                };

                elements.pop_front(); // pop the assignment operator
                let expression = Expression::try_from(elements.pop_front().ok_or(
                    AstError::InvalidStatement("Empty assignment expression".to_string()),
                )?)?;
                Ok(PlainStatement::Assignment { target, expression })
            }
            Rule::input_statement => {
                let mut elements = statement.clone().into_inner().collect::<VecDeque<_>>();
                if elements.is_empty() {
                    return Err(AstError::InvalidStatement(
                        "Invalid input statement: at least one argument is required".to_string(),
                    ));
                }

                let mut prompt = None;
                let mut variables = Vec::new();

                let first_expr = Expression::try_from(elements.pop_front().ok_or(
                    AstError::InvalidStatement("Empty input expression".to_string()),
                )?)?;
                match first_expr {
                    Expression::String(s) => prompt = Some(s),
                    Expression::Variable(v) => variables.push(v),
                    _ => {
                        return Err(AstError::InvalidStatement(format!(
                            "Invalid input expression: {first_expr:?}"
                        )));
                    }
                }

                while !elements.is_empty() {
                    let expr = Expression::try_from(elements.pop_front().ok_or(
                        AstError::InvalidStatement("Empty input expression".to_string()),
                    )?)?;
                    if let Expression::Variable(v) = expr {
                        variables.push(v);
                    } else {
                        return Err(AstError::InvalidStatement(format!(
                            "Invalid input expression: {expr:?}"
                        )));
                    }
                }
                Ok(PlainStatement::Input { prompt, variables })
            }
            Rule::comment => {
                let mut elements = statement.clone().into_inner().collect::<VecDeque<_>>();
                // Pop the remark leader (REM or ')
                let _ = elements
                    .pop_front()
                    .ok_or(AstError::InvalidStatement("Empty comment".to_string()))?;
                // Pop the metacommand if present, otherwise this is a noop
                match elements.pop_front().map(|p| p.as_str()) {
                    Some("$STATIC") => Ok(PlainStatement::Metacommand(Metacommand::Static)),
                    Some("$DYNAMIC") => Ok(PlainStatement::Metacommand(Metacommand::Dynamic)),
                    _ => Ok(PlainStatement::Noop),
                }
            }
            Rule::dim_statement => {
                let mut elements = statement.into_inner();

                // Skip optional SHARED keyword
                let mut current = elements.next().ok_or(AstError::InvalidStatement(
                    "Empty dim statement".to_string(),
                ))?;
                if current.as_rule() == Rule::shared_keyword {
                    current = elements.next().ok_or(AstError::InvalidStatement(
                        "Missing variable name in dim statement".to_string(),
                    ))?;
                }

                // Get variable name
                let name = current.as_str().to_string();

                // Parse array subscripts if present
                let mut dimensions = Vec::new();
                if let Some(subscripts_pair) = elements.next() {
                    if subscripts_pair.as_rule() == Rule::array_subscripts {
                        for subscript_pair in subscripts_pair.into_inner() {
                            let mut subscript_elements = subscript_pair.into_inner();

                            let first_expr =
                                Expression::try_from(subscript_elements.next().ok_or(
                                    AstError::InvalidStatement("Empty array subscript".to_string()),
                                )?)?;

                            // Check if this is a "TO" expression or just an upper bound
                            if let Some(second_expr) = subscript_elements.next() {
                                // This is "lower TO upper" format
                                dimensions.push(ArrayDimension {
                                    lower: Some(first_expr),
                                    upper: Expression::try_from(second_expr)?,
                                });
                            } else {
                                // This is just "upper" format (lower bound defaults to 0 or 1)
                                dimensions.push(ArrayDimension {
                                    lower: None,
                                    upper: first_expr,
                                });
                            }
                        }
                    }
                }

                // Parse type specifier if present
                let element_type = if let Some(type_pair) = elements.next() {
                    if type_pair.as_rule() == Rule::type_specifier {
                        match type_pair.as_str() {
                            "INTEGER" => VariableType::Integer,
                            "LONG" => VariableType::Long,
                            "SINGLE" => VariableType::SinglePrecision,
                            "DOUBLE" => VariableType::DoublePrecision,
                            "STRING" => VariableType::String,
                            _ => VariableType::Integer, // Default for user-defined types
                        }
                    } else {
                        VariableType::Integer // Default
                    }
                } else {
                    VariableType::Integer // Default
                };

                Ok(PlainStatement::Dim(ArrayDeclaration {
                    name,
                    element_type,
                    dimensions,
                }))
            }
            Rule::for_statement => {
                let mut elements = statement.into_inner();

                // Parse counter variable
                let counter = Variable::try_from(elements.next().ok_or(
                    AstError::InvalidStatement("Missing counter variable".to_string()),
                )?)?;

                // Parse start expression
                let start = Expression::try_from(elements.next().ok_or(
                    AstError::InvalidStatement("Missing start expression".to_string()),
                )?)?;

                // Parse end expression
                let end = Expression::try_from(elements.next().ok_or(
                    AstError::InvalidStatement("Missing end expression".to_string()),
                )?)?;

                // Parse optional step expression
                let mut step = None;
                let mut statements_element = elements.next().ok_or(AstError::InvalidStatement(
                    "Missing statement list".to_string(),
                ))?;

                // Check if the next element is a step expression or statement list
                if statements_element.as_rule() == Rule::expression {
                    step = Some(Expression::try_from(statements_element)?);
                    statements_element = elements.next().ok_or(AstError::InvalidStatement(
                        "Missing statement list after STEP".to_string(),
                    ))?;
                }

                // Parse statement list
                let statements = parse_statement_list(statements_element)?;

                // Skip optional NEXT variable (we don't validate it matches counter for now)

                Ok(PlainStatement::For {
                    counter,
                    start,
                    end,
                    step,
                    statements,
                })
            }
            Rule::if_statement => {
                let elements = statement.into_inner().collect::<Vec<_>>();
                let mut branches = Vec::new();
                let mut i = 0;

                // Parse IF branch: condition + statement_list
                if i + 1 < elements.len() {
                    let condition = Expression::try_from(elements[i].clone())?;
                    i += 1;
                    let statements = parse_statement_list(elements[i].clone())?;
                    i += 1;

                    branches.push(IfBranch {
                        condition: Some(condition),
                        statements,
                    });
                }

                // Parse ELSEIF branches: pairs of (condition, statement_list)
                while i + 1 < elements.len() {
                    let condition = Expression::try_from(elements[i].clone())?;
                    i += 1;
                    let statements = parse_statement_list(elements[i].clone())?;
                    i += 1;

                    branches.push(IfBranch {
                        condition: Some(condition),
                        statements,
                    });
                }

                // Parse ELSE branch: remaining statement_list (if any)
                if i < elements.len() {
                    let statements = parse_statement_list(elements[i].clone())?;

                    branches.push(IfBranch {
                        condition: None,
                        statements,
                    });
                }

                Ok(PlainStatement::If { branches })
            }
            Rule::do_statement => {
                // Delegate to the specific DO statement type
                let inner_statement = statement
                    .into_inner()
                    .next()
                    .ok_or(AstError::InvalidStatement("Empty DO statement".to_string()))?;
                PlainStatement::try_from(inner_statement)
            }
            Rule::do_while_statement => {
                let elements = statement.into_inner().collect::<Vec<_>>();

                // The structure should be: logical_expression, statement_list
                if elements.len() != 2 {
                    return Err(AstError::InvalidStatement(format!(
                        "Expected 2 elements in DO WHILE, got {}",
                        elements.len()
                    )));
                }

                // Parse condition
                let condition = Expression::try_from(elements[0].clone())?;

                // Parse statement list
                let statements = parse_statement_list(elements[1].clone())?;

                Ok(PlainStatement::DoLoop {
                    condition_type: DoConditionType::PreTestWhile,
                    condition: Some(condition),
                    statements,
                })
            }
            Rule::do_until_statement => {
                let elements = statement.into_inner().collect::<Vec<_>>();

                // The structure should be: logical_expression, statement_list
                if elements.len() != 2 {
                    return Err(AstError::InvalidStatement(format!(
                        "Expected 2 elements in DO UNTIL, got {}",
                        elements.len()
                    )));
                }

                // Parse condition
                let condition = Expression::try_from(elements[0].clone())?;

                // Parse statement list
                let statements = parse_statement_list(elements[1].clone())?;

                Ok(PlainStatement::DoLoop {
                    condition_type: DoConditionType::PreTestUntil,
                    condition: Some(condition),
                    statements,
                })
            }
            Rule::do_loop_while_statement => {
                let elements = statement.into_inner().collect::<Vec<_>>();

                // The structure should be: statement_list, logical_expression
                if elements.len() != 2 {
                    return Err(AstError::InvalidStatement(format!(
                        "Expected 2 elements in DO LOOP WHILE, got {}",
                        elements.len()
                    )));
                }

                // Parse statement list
                let statements = parse_statement_list(elements[0].clone())?;

                // Parse condition
                let condition = Expression::try_from(elements[1].clone())?;

                Ok(PlainStatement::DoLoop {
                    condition_type: DoConditionType::PostTestWhile,
                    condition: Some(condition),
                    statements,
                })
            }
            Rule::do_loop_until_statement => {
                let elements = statement.into_inner().collect::<Vec<_>>();

                // The structure should be: statement_list, logical_expression
                if elements.len() != 2 {
                    return Err(AstError::InvalidStatement(format!(
                        "Expected 2 elements in DO LOOP UNTIL, got {}",
                        elements.len()
                    )));
                }

                // Parse statement list
                let statements = parse_statement_list(elements[0].clone())?;

                // Parse condition
                let condition = Expression::try_from(elements[1].clone())?;

                Ok(PlainStatement::DoLoop {
                    condition_type: DoConditionType::PostTestUntil,
                    condition: Some(condition),
                    statements,
                })
            }
            Rule::do_infinite_statement => {
                let elements = statement.into_inner().collect::<Vec<_>>();

                // The structure should be: statement_list only
                if elements.len() != 1 {
                    return Err(AstError::InvalidStatement(format!(
                        "Expected 1 element in DO LOOP, got {}",
                        elements.len()
                    )));
                }

                // Parse statement list
                let statements = parse_statement_list(elements[0].clone())?;

                Ok(PlainStatement::DoLoop {
                    condition_type: DoConditionType::None,
                    condition: None,
                    statements,
                })
            }
            Rule::while_statement => {
                let elements = statement.into_inner().collect::<Vec<_>>();

                // The structure should be: logical_expression, statement_list
                if elements.len() != 2 {
                    return Err(AstError::InvalidStatement(format!(
                        "Expected 2 elements in WHILE, got {}",
                        elements.len()
                    )));
                }

                // Parse condition
                let condition = Expression::try_from(elements[0].clone())?;

                // Parse statement list
                let statements = parse_statement_list(elements[1].clone())?;

                Ok(PlainStatement::While { condition, statements })
            }
            Rule::exit_statement => {
                // The exit_statement rule matches "EXIT" followed by "DO" or "FOR"
                // We need to check the text content to determine which type
                let text = statement.as_str();
                if text.contains("EXIT DO") {
                    Ok(PlainStatement::ExitDo)
                } else if text.contains("EXIT FOR") {
                    Ok(PlainStatement::ExitFor)
                } else {
                    Err(AstError::InvalidStatement(format!(
                        "Invalid EXIT statement: {}",
                        text
                    )))
                }
            }
            Rule::goto_statement => {
                let mut elements = statement.into_inner();
                let target_pair = elements.next().ok_or(
                    AstError::InvalidStatement("Missing GOTO target".to_string()),
                )?;

                let target = match target_pair.as_rule() {
                    Rule::number => {
                        let line_number = target_pair.as_str().parse::<u32>().map_err(|_| {
                            AstError::InvalidStatement("Invalid line number in GOTO".to_string())
                        })?;
                        GotoTarget::LineNumber(line_number)
                    }
                    Rule::identifier => {
                        GotoTarget::Label(target_pair.as_str().to_string())
                    }
                    _ => {
                        return Err(AstError::InvalidStatement(
                            "GOTO target must be line number or label".to_string(),
                        ));
                    }
                };

                Ok(PlainStatement::Goto(target))
            }
            _ => Err(AstError::InvalidStatement(format!(
                "Expected plain statement, got {:?}",
                statement.as_rule()
            ))),
        }
    }
}

fn parse_statement_list(statement_list: Pair<'_, Rule>) -> Result<Vec<Statement>, AstError> {
    statement_list
        .into_inner()
        .map(|plain_stmt_pair| Ok(Statement::PlainStatement(PlainStatement::try_from(plain_stmt_pair)?)))
        .collect::<Result<Vec<_>, _>>()
}

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(i32),
    Float(f64),
    String(String),
    BinaryOp {
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },
    UnaryOp {
        op: UnaryOperator,
        operand: Box<Expression>,
    },
    LogicalOp {
        left: Option<Box<Expression>>, // None for NOT operator
        op: LogicalOperator,
        right: Box<Expression>,
    },
    RelationalOp {
        left: Box<Expression>,
        op: RelationalOperator,
        right: Box<Expression>,
    },
    Variable(Variable),
    ArrayAccess(ArrayAccess),
}

impl TryFrom<Pair<'_, Rule>> for Expression {
    type Error = AstError;

    fn try_from(expr: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match expr.as_rule() {
            Rule::logical_expression => {
                let mut elements = expr.clone().into_inner().collect::<Vec<_>>();

                if elements.is_empty() {
                    return Err(AstError::InvalidExpression(
                        "Empty logical expression".to_string(),
                    ));
                }

                // Start with the first logical term
                let mut result = Expression::try_from(elements.remove(0))?;

                // Process remaining OR operator-term pairs
                while elements.len() >= 2 {
                    let op = LogicalOperator::try_from(elements.remove(0))?;
                    let right = Expression::try_from(elements.remove(0))?;

                    result = Expression::LogicalOp {
                        left: Some(Box::new(result)),
                        op,
                        right: Box::new(right),
                    };
                }

                Ok(result)
            }
            Rule::logical_term => {
                let mut elements = expr.clone().into_inner().collect::<Vec<_>>();

                if elements.is_empty() {
                    return Err(AstError::InvalidExpression(
                        "Empty logical term".to_string(),
                    ));
                }

                // Start with the first logical factor
                let mut result = Expression::try_from(elements.remove(0))?;

                // Process remaining AND operator-factor pairs
                while elements.len() >= 2 {
                    let op = LogicalOperator::try_from(elements.remove(0))?;
                    let right = Expression::try_from(elements.remove(0))?;

                    result = Expression::LogicalOp {
                        left: Some(Box::new(result)),
                        op,
                        right: Box::new(right),
                    };
                }

                Ok(result)
            }
            Rule::logical_factor => {
                let mut elements = expr.clone().into_inner().collect::<Vec<_>>();

                if elements.is_empty() {
                    return Err(AstError::InvalidExpression(
                        "Empty logical factor".to_string(),
                    ));
                }

                // Check if this starts with NOT
                let first = &elements[0];
                if first.as_rule() == Rule::not_operator {
                    if elements.len() < 2 {
                        return Err(AstError::InvalidExpression(
                            "NOT operator without operand".to_string(),
                        ));
                    }
                    let op = LogicalOperator::try_from(elements.remove(0))?;
                    let operand = Expression::try_from(elements.remove(0))?;
                    Ok(Expression::LogicalOp {
                        left: None,
                        op,
                        right: Box::new(operand),
                    })
                } else {
                    // Just parse the relational expression
                    Expression::try_from(elements.remove(0))
                }
            }
            Rule::relational_expression => {
                let mut elements = expr.clone().into_inner().collect::<Vec<_>>();

                if elements.is_empty() {
                    return Err(AstError::InvalidExpression(
                        "Empty relational expression".to_string(),
                    ));
                }

                // If there's only one element, it's just an expression
                if elements.len() == 1 {
                    return Expression::try_from(elements.remove(0));
                }

                // Otherwise, it's left relational_operator right
                if elements.len() != 3 {
                    return Err(AstError::InvalidExpression(
                        "Invalid relational expression format".to_string(),
                    ));
                }

                let left = Expression::try_from(elements.remove(0))?;
                let op = RelationalOperator::try_from(elements.remove(0))?;
                let right = Expression::try_from(elements.remove(0))?;

                Ok(Expression::RelationalOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                })
            }
            Rule::expression => {
                let mut elements = expr.clone().into_inner().collect::<Vec<_>>();

                if elements.is_empty() {
                    return Err(AstError::InvalidExpression("Empty expression".to_string()));
                }

                // Start with the first term
                let mut result = Expression::try_from(elements.remove(0))?;

                // Process remaining operator-term pairs
                while elements.len() >= 2 {
                    let op = BinaryOperator::try_from(elements.remove(0))?;
                    let right = Expression::try_from(elements.remove(0))?;

                    result = Expression::BinaryOp {
                        left: Box::new(result),
                        op,
                        right: Box::new(right),
                    };
                }

                Ok(result)
            }
            Rule::term => {
                let mut elements = expr.clone().into_inner().collect::<Vec<_>>();

                if elements.is_empty() {
                    return Err(AstError::InvalidExpression("Empty term".to_string()));
                }

                // Start with the first factor
                let mut result = Expression::try_from(elements.remove(0))?;

                // Process remaining operator-factor pairs
                while elements.len() >= 2 {
                    let op = BinaryOperator::try_from(elements.remove(0))?;
                    let right = Expression::try_from(elements.remove(0))?;

                    result = Expression::BinaryOp {
                        left: Box::new(result),
                        op,
                        right: Box::new(right),
                    };
                }

                Ok(result)
            }
            Rule::variable => Ok(Expression::Variable(Variable::try_from(expr)?)),
            Rule::factor => {
                let mut elements = expr.clone().into_inner().collect::<Vec<_>>();

                if elements.is_empty() {
                    return Err(AstError::InvalidExpression("Empty factor".to_string()));
                }

                // Check if there's a unary operator
                if elements.len() == 2 {
                    let op = UnaryOperator::try_from(elements.remove(0))?;
                    let operand = Expression::try_from(elements.remove(0))?;
                    Ok(Expression::UnaryOp {
                        op,
                        operand: Box::new(operand),
                    })
                } else {
                    // Just parse the primary expression
                    Expression::try_from(elements.remove(0))
                }
            }
            Rule::primary => {
                let mut inner = expr.clone().into_inner();
                Ok(Expression::try_from(inner.next().unwrap())?)
            }
            Rule::array_access => Ok(Expression::ArrayAccess(ArrayAccess::try_from(expr)?)),
            Rule::number => {
                let number = expr.as_str();
                if number.contains('.') {
                    Ok(Expression::Float(number.parse::<f64>().map_err(|e| {
                        AstError::InvalidExpression(format!("Invalid float: {e}"))
                    })?))
                } else {
                    Ok(Expression::Integer(number.parse::<i32>().map_err(|e| {
                        AstError::InvalidExpression(format!("Invalid integer: {e}"))
                    })?))
                }
            }
            Rule::string => {
                // Don't store the string with the quotes
                let string = expr.as_str();
                Ok(Expression::String(string[1..string.len() - 1].to_string()))
            }
            _ => Err(AstError::InvalidExpression(format!(
                "Expected integer, got {:?}",
                expr.as_rule()
            ))),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Plus,
    Minus,
}

#[derive(Debug, Clone)]
pub enum LogicalOperator {
    And,
    Or,
    Not,
}

#[derive(Debug, Clone)]
pub enum RelationalOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

impl TryFrom<Pair<'_, Rule>> for BinaryOperator {
    type Error = AstError;

    fn try_from(op: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match op.as_rule() {
            Rule::addition_operator => Ok(BinaryOperator::Add),
            Rule::subtraction_operator => Ok(BinaryOperator::Subtract),
            Rule::multiplication_operator => Ok(BinaryOperator::Multiply),
            Rule::division_operator => Ok(BinaryOperator::Divide),
            _ => Err(AstError::InvalidExpression(format!(
                "Invalid binary operator: {op:?}"
            ))),
        }
    }
}

impl TryFrom<Pair<'_, Rule>> for UnaryOperator {
    type Error = AstError;

    fn try_from(op: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match op.as_rule() {
            Rule::unary_operator => match op.as_str() {
                "+" => Ok(UnaryOperator::Plus),
                "-" => Ok(UnaryOperator::Minus),
                _ => Err(AstError::InvalidExpression(format!(
                    "Invalid unary operator: {}",
                    op.as_str()
                ))),
            },
            _ => Err(AstError::InvalidExpression(format!(
                "Invalid unary operator rule: {op:?}"
            ))),
        }
    }
}

impl TryFrom<Pair<'_, Rule>> for LogicalOperator {
    type Error = AstError;

    fn try_from(op: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match op.as_rule() {
            Rule::and_operator => Ok(LogicalOperator::And),
            Rule::or_operator => Ok(LogicalOperator::Or),
            Rule::not_operator => Ok(LogicalOperator::Not),
            _ => Err(AstError::InvalidExpression(format!(
                "Invalid logical operator: {op:?}"
            ))),
        }
    }
}

impl TryFrom<Pair<'_, Rule>> for RelationalOperator {
    type Error = AstError;

    fn try_from(op: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match op.as_rule() {
            Rule::relational_operator => match op.as_str() {
                "=" => Ok(RelationalOperator::Equal),
                "<>" => Ok(RelationalOperator::NotEqual),
                "<" => Ok(RelationalOperator::LessThan),
                "<=" => Ok(RelationalOperator::LessThanEqual),
                ">" => Ok(RelationalOperator::GreaterThan),
                ">=" => Ok(RelationalOperator::GreaterThanEqual),
                _ => Err(AstError::InvalidExpression(format!(
                    "Invalid relational operator: {}",
                    op.as_str()
                ))),
            },
            _ => Err(AstError::InvalidExpression(format!(
                "Invalid relational operator rule: {op:?}"
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariableType {
    // Suffix: $
    String,
    // Suffix !
    SinglePrecision,
    // Suffix #
    DoublePrecision,
    // Suffix %
    Integer, // default
    // Suffix &
    Long,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable {
    pub name: String,
    pub r#type: VariableType,
}

impl TryFrom<Pair<'_, Rule>> for ArrayAccess {
    type Error = AstError;

    fn try_from(value: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        match value.as_rule() {
            Rule::array_access => {
                let full_text = value.as_str();

                // Extract array name from the full text (everything before the opening parenthesis)
                let name = full_text
                    .split('(')
                    .next()
                    .ok_or(AstError::InvalidExpression(
                        "Invalid array access format".to_string(),
                    ))?
                    .to_string();

                // Get index expressions from the inner elements
                let indices = value
                    .into_inner()
                    .map(Expression::try_from)
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(ArrayAccess { name, indices })
            }
            _ => Err(AstError::InvalidExpression(format!(
                "Expected array_access, got {:?}",
                value.as_rule()
            ))),
        }
    }
}

impl TryFrom<Pair<'_, Rule>> for Variable {
    type Error = AstError;

    fn try_from(value: Pair<'_, Rule>) -> Result<Self, Self::Error> {
        let full_text = value.as_str();

        if full_text.is_empty() {
            return Err(AstError::InvalidStatement(
                "Variable name cannot be empty".to_string(),
            ));
        }

        if full_text.len() > 40 {
            return Err(AstError::InvalidStatement(
                "Variable name too long".to_string(),
            ));
        }

        // Parse the variable name and type suffix
        let (name, r#type) = if let Some(stripped) = full_text.strip_suffix('!') {
            (stripped.to_string(), VariableType::SinglePrecision)
        } else if let Some(stripped) = full_text.strip_suffix('#') {
            (stripped.to_string(), VariableType::DoublePrecision)
        } else if let Some(stripped) = full_text.strip_suffix('$') {
            (stripped.to_string(), VariableType::String)
        } else if let Some(stripped) = full_text.strip_suffix('%') {
            (stripped.to_string(), VariableType::Integer)
        } else if let Some(stripped) = full_text.strip_suffix('&') {
            (stripped.to_string(), VariableType::Long)
        } else {
            (full_text.to_string(), VariableType::Integer) // Default type
        };

        if name.is_empty() {
            return Err(AstError::InvalidStatement(
                "Variable name cannot be empty".to_string(),
            ));
        }

        Ok(Variable { name, r#type })
    }
}

#[derive(Debug, Clone)]
pub enum Metacommand {
    Dynamic,
    Static,
}
