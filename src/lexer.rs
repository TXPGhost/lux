use logos::Logos;

#[derive(Logos, Clone, Copy, Debug, PartialEq)]
#[logos(skip r" ")]
pub enum Token {
    #[regex(r"(?:    |\t)")]
    Indent,

    #[regex(r"[\n\f]")]
    Newline,

    #[token(",")]
    Comma,

    #[regex(r"[a-z][a-zA-Z\d]*")]
    Variable,

    #[regex(r"[A-Z][a-zA-Z\d]*]")]
    Class,

    #[regex(r"([\d]*\.[\d]+)|([\d]+)")]
    Number,

    #[token("_")]
    Underscore,

    #[token(".")]
    Dot,

    #[token("=")]
    Equals,

    #[token("==")]
    EqualsEquals,

    #[token("+")]
    Plus,

    #[token("++")]
    PlusPlus,

    #[token("-")]
    Minus,

    #[token("--")]
    MinusMinus,

    #[token("*")]
    Asterisk,

    #[token("**")]
    AsteriskAsterisk,

    #[token("/")]
    Slash,

    #[token("//")]
    SlashSlash,

    #[token("&")]
    Ampersand,

    #[token("&&")]
    AmpersandAmpersand,

    #[token("|")]
    Pipe,

    #[token("||")]
    PipePipe,

    #[token("~")]
    Tilde,

    #[token("!")]
    Bang,

    #[token("!!")]
    BangBang,

    #[token("?")]
    Question,

    #[token("#")]
    Pound,

    #[token("%")]
    Percent,

    #[token("^")]
    Caret,

    #[token(":")]
    Colon,

    #[token("::")]
    ColonColon,

    #[token("\\")]
    Backslash,

    #[token("->")]
    ThinArrow,

    #[token("=>")]
    FatArrow,

    #[token("!=")]
    BangEquals,

    #[token("<")]
    LAngle,

    #[token(">")]
    RAngle,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LSquare,

    #[token("]")]
    RSquare,

    #[token("{")]
    LCurl,

    #[token("}")]
    RCurl,

    #[token("<=")]
    LAngleEquals,

    #[token(">=")]
    RAngleEquals,

    #[token("<=>")]
    LAngleEqualsRAngle,
}
