//! Source-only layout metadata. Nothing here is retained by the evaluator or AST caches.
use crate::lexer::{Lexer, Token};
use crate::parser::{ParseError, parse_source};

/// Format indentation without rewriting tokens, comments, or literal contents.
/// The AST printer serves generated code; user source needs a lossless layout pass.
pub fn format_source(source: &str) -> Result<String, ParseError>
{
    parse_source(source)?;
    let lines: Vec<&str> = source.split_inclusive('\n').collect();
    let protected = protected_lines(source);
    let mut indent = vec![None; lines.len()];
    let mut after = vec![None; lines.len()];
    let mut depth = 0usize;
    let mut lexer = Lexer::new(source);
    let mut previous = Token::EOF;
    loop
    {
        let span = lexer.next_token()?;
        if span.token == Token::EOF
        {
            break;
        }
        let member = matches!(previous, Token::Dot | Token::ColonColon);
        let closes = matches!(
            span.token,
            Token::End | Token::RightBrace | Token::RightBracket | Token::RightParen
        ) && !member;
        let branch = matches!(span.token, Token::Else | Token::Elif) && !member;
        let line = span.line - 1;
        if closes
        {
            depth = depth.saturating_sub(1);
        }
        if indent[line].is_none()
        {
            indent[line] = Some(depth.saturating_sub(usize::from(branch)));
        }
        if !member
            && matches!(
                span.token,
                Token::Fn
                    | Token::If
                    | Token::While
                    | Token::For
                    | Token::Loop
                    | Token::Collect
                    | Token::LeftBrace
                    | Token::LeftBracket
                    | Token::LeftParen
            )
        {
            depth += 1;
        }
        after[line] = Some(depth);
        previous = span.token;
    }
    let mut output = String::with_capacity(source.len());
    let mut depth = 0;
    for (index, line) in lines.iter().enumerate()
    {
        if protected[index] || line.trim().is_empty()
        {
            output.push_str(line);
        }
        else
        {
            output.push_str(&"  ".repeat(indent[index].unwrap_or(depth)));
            output.push_str(line.trim_start_matches([' ', '\t']));
        }
        if let Some(next) = after[index]
        {
            depth = next;
        }
    }
    Ok(output)
}

/// Mark lines whose leading whitespace belongs to a multiline literal/comment.
/// This auxiliary scan is used only by formatting, never normal parsing/execution.
fn protected_lines(source: &str) -> Vec<bool>
{
    let mut protected = vec![false];
    let mut chars = source.chars().peekable();
    let mut quote = None;
    let mut comment_depth = 0usize;
    let mut line_comment = false;
    let mut escaped = false;
    while let Some(ch) = chars.next()
    {
        if ch == '\n'
        {
            protected.push(quote.is_some() || comment_depth > 0);
            line_comment = false;
            escaped = false;
            continue;
        }
        if line_comment
        {
            continue;
        }
        if let Some(delimiter) = quote
        {
            if escaped
            {
                escaped = false;
            }
            else if ch == '\\'
            {
                escaped = true;
            }
            else if ch == delimiter
            {
                quote = None;
            }
            continue;
        }
        if ch == '(' && chars.peek() == Some(&'*')
        {
            chars.next();
            comment_depth += 1;
        }
        else if comment_depth > 0
        {
            if ch == '*' && chars.peek() == Some(&')')
            {
                chars.next();
                comment_depth -= 1;
            }
        }
        else if ch == '#'
        {
            line_comment = true;
        }
        else if ch == '"' || ch == '`'
        {
            quote = Some(ch);
        }
    }
    protected
}

#[cfg(test)]
mod tests
{
    use super::*;

    fn tokens(source: &str) -> Vec<Token>
    {
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop
        {
            let token = lexer.next_token().unwrap().token;
            if token == Token::EOF
            {
                return tokens;
            }
            tokens.push(token);
        }
    }

    #[test]
    fn preserves_comments_literals_and_is_idempotent()
    {
        let source = "# header\nfn demo()\n# explanation\nputs \"first\n  # literal text\nlast\" # trailing\n(* outer\n  (* nested *)\n unchanged *)\nif true\nputs 1\nelse\nputs 2\nend\nend\n# footer\n";
        let formatted = format_source(source).unwrap();
        assert!(formatted.contains("  # explanation\n"));
        assert!(formatted.contains("  if true\n    puts 1\n  else\n    puts 2\n  end"));
        assert!(formatted.contains("(* outer\n  (* nested *)\n unchanged *)"));
        assert!(formatted.ends_with("# footer\n"));
        assert_eq!(tokens(source), tokens(&formatted));
        assert_eq!(formatted, format_source(&formatted).unwrap());
    }

    #[test]
    fn preserves_line_endings_and_comment_only_source()
    {
        for source in [
            "# only\r\n",
            "(* first\n  second *)",
            "",
            "puts `echo hi\n  echo there`\n",
        ]
        {
            assert_eq!(source, format_source(source).unwrap());
        }
        assert!(format_source("end ignored").is_err());
    }
}
