CodeMirror.defineMode("elixir", function(config) {
    function wordObj(words) {
        var o = {};
        for (var i = 0, e = words.length; i < e; ++i) o[words[i]] = true;
        return o;
    }
    var keywords = wordObj([
        "->",
        "bc",
        "lc",
        "in",
        "inbits",
        "inlist",
        "quote",
        "unquote",
        "unquote_splicing",
        "var",
        "do",
        "after",
        "for",
        "def",
        "defdelegate",
        "defimpl",
        "defmacro",
        "defmacrop",
        "defmodule",
        "defoverridable",
        "defp",
        "defprotocol",
        "defrecord",
        "destructure",
        "alias",
        "require",
        "import",
        "use",
        "if",
        "when",
        "case",
        "cond",
        "throw",
        "then",
        "else",
        "elsif",
        "try",
        "catch",
        "rescue",
        "fn",
        "function",
        "receive",
        "end",
    ]);
        var atomWords = wordObj([
        "true",
        "false",
        "nil"
    ]);
    var indentWords = wordObj([
        "case",
        "catch",
        "def",
        "defdelegate",
        "defimpl",
        "defmacro",
        "defmacrop",
        "defmodule",
        "defoverridable",
        "defp",
        "defprotocol",
        "defrecord",
        // "do",
        "for",
        "then",
        "->",
    ]);
        var defineWords = wordObj([
        "def",
        "defdelegate",
        "defmacro",
        "defmacrop",
        "defoverridable",
        "defp"
    ]);
    var moduleWords = wordObj([
        "Behavior",
        "Binary",
        "Bitwise",
        "Builtin",
        "Elixir",
        "Code",
        "EEx",
        "Enum",
        "ExUnit",
        "Exception",
        "File",
        "GenServer",
        "Function",
        "GenServer",
        "GenTCP",
        "HashDict",
        "IO",
        "Keyword",
        "List",
        "Math",
        "Module",
        "Node",
        "OptionParser",
        "OrdDict",
        "Port",
        "Process",
        "Record",
        "Regexp",
        "System",
        "Tuple",
        "URI",
        "UnboundMethod"
    ]);
    var builtinWords = wordObj([
        "__MODULE__",
        "__LINE__",
        "__FILE__",
        "__ENV__",
        "__CALLER__"
    ]);
    var operatorWords = wordObj([
        "+",
        "++",
        "<>",
        "-",
        "/",
        "*",
        "div",
        "rem",
        "==",
        "!=",
        "<=",
        "<",
        ">=",
        ">",
        "===",
        "!==",
        "and",
        "or",
        "not",
        "&&",
        "||",
        "!",
        ".",
        "#",
        "=",
        ":=",
        "<-",
        "//",
        "\\"
    ]);
    var dedentWords = wordObj(["end", "until"]);
    var matching = {"[": "]", "{": "}", "(": ")"};
    var curPunc;

    function chain(newtok, stream, state) {
        state.tokenize.push(newtok);
        return newtok(stream, state);
    }

    function tokenBase(stream, state) {
        curPunc = null;
        if (stream.sol() && stream.match("=begin") && stream.eol()) {
            state.tokenize.push(readBlockComment);
            return "comment";
        }
        if (stream.eatSpace()) return null;
        var ch = stream.next(), m;
        if (ch == "'" || ch == '"') {
            return chain(readQuoted(ch, "string", ch == '"' || ch == '"'), stream, state);
        } else if (ch == "%") {
            var style = "string", embed = true;
            if (stream.eat(/[SWC]/)) style = "string";
            else if (stream.eat(/[r]/)) style = "string-2";
                else if (stream.eat(/[swc]/)) { style = "string"; embed = false; }
            var delim = stream.eat(/[^\w\s]/);
            if (!delim) return "operator";
            if (matching.propertyIsEnumerable(delim)) delim = matching[delim];
            return chain(readQuoted(delim, style, embed, true), stream, state);
        } else if (ch == "#") {
            stream.skipToEnd();
            return "comment";
        } else if (ch == "0") {
            if (stream.eat("x")) stream.eatWhile(/[\da-fA-F]/);
            else if (stream.eat("b")) stream.eatWhile(/[01]/);
                else stream.eatWhile(/[0-7]/);
            return "number";
        } else if (/\d/.test(ch)) {
            stream.match(/^[\d_]*(?:\.[\d_]+)?(?:[eE][+\-]?[\d_]+)?/);
            return "number";
        } else if (ch == ":") {
            if (stream.eat("'")) return chain(readQuoted("'", "atom", false), stream, state);
            if (stream.eat('"')) return chain(readQuoted('"', "atom", true), stream, state);
            if (stream.eat(/[a-zA-Z_]/)) {
                stream.eatWhile(/[\w]/);
                stream.eat(/[\?\!\=]/);
                return "atom";
            }
            return "operator";
        } else if (/[a-zA-Z_]/.test(ch)) {
            stream.eatWhile(/[\w]/);
            stream.eat(/[\?\!]/);
            if (stream.eat(":")) return "atom";
            return "ident";
        } else if (/[\(\)\[\]{}\\;]/.test(ch)) {
            curPunc = ch;
            return null;
        } else if (ch == "-" && stream.eat(">")) {
            return "ident";
        } else if (/[=+\-\/*:\.^%<>~|&]/.test(ch)) {
            stream.eatWhile(/[=+\-\/*:\.^%<>~|&]/);
            return "operator";
        } else {
            return null;
        }
    }

    function tokenBaseUntilBrace() {
        var depth = 1;
        return function(stream, state) {
            if (stream.peek() == "}") {
                depth--;
                if (depth === 0) {
                    state.tokenize.pop();
                    return state.tokenize[state.tokenize.length - 1](stream, state);
                }
            } else if (stream.peek() == "{") {
                depth++;
            }
            return tokenBase(stream, state);
        };
    }
    function tokenBaseOnce() {
        var alreadyCalled = false;
        return function(stream, state) {
            if (alreadyCalled) {
                state.tokenize.pop();
                return state.tokenize[state.tokenize.length - 1](stream, state);
            }
            alreadyCalled = true;
            return tokenBase(stream, state);
        };
    }
    function readQuoted(quote, style, embed, unescaped) {
        return function(stream, state) {
            var escaped = false, ch;
            if (state.context.type === 'read-quoted-paused') {
                state.context = state.context.prev;
                stream.eat("}");
            }
            ch = stream.next();
            while (ch !== null && ch !== undefined) {
                if (ch == quote && (unescaped || !escaped)) {
                    state.tokenize.pop();
                    break;
                }
                if (embed && ch == "#" && !escaped) {
                    if (stream.eat("{")) {
                        if (quote == "}") {
                            state.context = {prev: state.context, type: 'read-quoted-paused'};
                        }
                        state.tokenize.push(tokenBaseUntilBrace());
                        break;
                    }
                }
                escaped = !escaped && ch == "\\";
                ch = stream.next();
            }
            return style;
        };
    }
    function readHereDoc(phrase) {
        return function(stream, state) {
            if (stream.match(phrase)) state.tokenize.pop();
            else stream.skipToEnd();
            return "string";
        };
    }
    function readBlockComment(stream, state) {
        if (stream.sol() && stream.match("=end") && stream.eol())
            state.tokenize.pop();
        stream.skipToEnd();
        return "comment";
    }

    return {
        startState: function() {
            return {tokenize: [tokenBase],
                    indented: 0,
                    context: {type: "top", indented: -config.indentUnit},
                    continuedLine: false,
                    lastTok: null,
                    inArrowBlock: false};
        },

        token: function(stream, state) {
            if (stream.sol()) state.indented = stream.indentation();
            var style = state.tokenize[state.tokenize.length-1](stream, state), kwtype;
            var word;
            if (style == "ident") {
                word = stream.current();
                style = keywords.propertyIsEnumerable(stream.current()) ? "keyword"
                : atomWords.propertyIsEnumerable(stream.current()) ? "atom"
                : builtinWords.propertyIsEnumerable(stream.current()) ? "builtin"
                : operatorWords.propertyIsEnumerable(stream.current()) ? "operator"
                : moduleWords.propertyIsEnumerable(stream.current()) ? "tag"
                : /^[A-Z]/.test(word) ? "tag"
                : defineWords.propertyIsEnumerable(state.lastTok) ? "def"
                : "variable";
                if (indentWords.propertyIsEnumerable(word)) kwtype = "indent";
                else if (dedentWords.propertyIsEnumerable(word)) kwtype = "dedent";
            }
            if (curPunc || (style && style != "comment")) state.lastTok = word || curPunc || style;

            if (kwtype == "indent" || /[\(\[\{]/.test(curPunc))
                state.context = {prev: state.context, type: curPunc || style, indented: state.indented};
            else if ((kwtype == "dedent" || /[\)\]\}]/.test(curPunc)) && state.context.prev)
                state.context = state.context.prev;

            if (word === "->") {
                if (state.inArrowBlock) {
                    state.context = state.context.prev;
                } else {
                    state.inArrowBlock = true;
                }
            } else if (state.inArrowBlock && word === "end") {
                state.inArrowBlock = false;
                state.context = state.context.prev;
            }

            if (stream.eol())
                state.continuedLine = (curPunc == "\\" || style == "operator");
            return style;
        },

        indent: function(state, textAfter) {
            if (state.tokenize[state.tokenize.length-1] != tokenBase) return 0;
            var firstChar = textAfter && textAfter.charAt(0);
            var ct = state.context;
            var closing = ct.type == matching[firstChar] ||
                ct.type == "keyword" && /^(?:end|until|else|elsif|when|rescue)\b/.test(textAfter);
            var closingArrowBlock = state.inArrowBlock && /end$/.test(textAfter);
            var arrowEnding = state.lastTok !== "do" && /\->/.test(textAfter);
            return ct.indented +
                (closing ? 0 : config.indentUnit) +
                (closingArrowBlock || arrowEnding ? -config.indentUnit : 0) +
                (state.continuedLine ? config.indentUnit : 0);
        },

        // // // commented for now
        //electricChars: "}de>", // enD, rescuE, ->
        electricChars: "}>", // enD, rescuE, ->
        lineComment: "#"
    };
});

CodeMirror.defineMIME("text/x-elixir", "elixir");