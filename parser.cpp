#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <cctype>
#include <map>
#include <unordered_map>

using namespace std;

enum TokenType
{
    T_INT,
    T_FLOAT,
    T_CHAR,
    T_ID,
    T_NUM,
    T_AGAR,
    T_ELSE,
    T_GE,
    T_WHILE,
    T_FOR,
    T_VOID,
    T_RETURN,
    T_ASSIGN,
    T_PLUS,
    T_MINUS,
    T_MUL,
    T_DIV,
    T_STRING,
    T_LPAREN,
    T_RPAREN,
    T_LBRACE,
    T_RBRACE,
    T_SEMICOLON,
    T_GT,
    T_LT,
    T_EOF,
    T_COMMA,
    T_PERIOD,
    T_PRINT,
    T_IF,
    T_BOOL,
    T_FUNCTION,
    T_EQ,
    T_LE,
    T_AND
};

string tokenTypeToString(TokenType type)
{
    switch (type)
    {
    case T_IF:
        return "IF";
    case T_PRINT:
        return "PRINT";
    case T_CHAR:
        return "CHAR";
    case T_ID:
        return "ID";
    case T_EQ:
        return "EQ";
    case T_LE:
        return "LE";
    case T_LT:
        return "LT";
    case T_GE:
        return "GE";
    case T_GT:
        return "GT";
    case T_INT:
        return "INT";
    case T_EOF:
        return "EOF";
    case T_AND:
        return "AND";
    case T_FOR:
        return "FOR";
    case T_MUL:
        return "MUL";
    case T_DIV:
        return "DIV";
    case T_NUM:
        return "NUM";
    case T_ELSE:
        return "ELSE";
    case T_BOOL:
        return "BOOL";
    case T_FUNCTION:
        return "FUNCTION";
    case T_PLUS:
        return "PLUS";
    case T_MINUS:
        return "MINUS";
    case T_FLOAT:
        return "FLOAT";
    case T_WHILE:
        return "WHILE";
    case T_RETURN:
        return "RETURN";
    case T_ASSIGN:
        return "ASSIGN";
    case T_LPAREN:
        return "LPAREN";
    case T_RPAREN:
        return "RPAREN";
    case T_LBRACE:
        return "LBRACE";
    case T_RBRACE:
        return "RBRACE";
    case T_STRING:
        return "STRING";
    case T_SEMICOLON:
        return "SEMICOLON";
    default:
        return "Unknown Token";
    };
}
struct Token
{
    TokenType type;
    string value;
    int line; // Line number for the token
};

struct Symbol
{
    string type;    // Type of the identifier (e.g., "int", "float")
    string name;    // Name of the identifier
    int scopeLevel; // Scope level, if needed (for nested scopes)
    Symbol() : type(""), name(""), scopeLevel(0) {}
    Symbol(string t, string n, int s) : type(t), name(n), scopeLevel(s) {}
};

class SymbolTable
{
private:
    unordered_map<string, Symbol> table;

public:
    // Add symbol to the symbol table
    void addSymbol(const string &name, const string &type, int scopeLevel)
    {
        // Check if the symbol already exists
        if (table.find(name) != table.end())
        {
            cerr << "Error: Variable '" << name << "' is already defined in the current scope." << endl;
            exit(1); // Exit the program if the symbol is redefined
        }
        // Add new symbol to the table
        table[name] = Symbol(type, name, scopeLevel);
    }

    // Get a symbol from the symbol table
    Symbol *getSymbol(const string &name)
    {
        // Return a pointer to the symbol if it exists
        if (table.find(name) != table.end())
        {
            return &table[name];
        }
        return nullptr; // Return nullptr if the symbol is not found
    }
    void printTable()
    {
        if (table.empty())
        {
            cout << "Symbol table is empty." << endl;
            return;
        }

        cout << "Symbol Table:" << endl;
        cout << "--------------------------------" << endl;
        cout << "Name\tType\tScope Level" << endl;
        cout << "--------------------------------" << endl;

        for (const auto &entry : table)
        {
            cout << entry.second.name << "\t" << entry.second.type << "\t" << entry.second.scopeLevel << endl;
        }
        cout << "--------------------------------" << endl;
    }

    // Check if a symbol exists in the symbol table
    bool symbolExists(const string &name)
    {
        return table.find(name) != table.end();
    }
};

struct TAC
{
    string op;     // Operator
    string arg1;   // First operand
    string arg2;   // Second operand
    string result; // Result variable

    TAC(string o, string a1, string a2, string res) : op(o), arg1(a1), arg2(a2), result(res) {}
};

class AssemblyGenerator
{
private:
    vector<string> assemblyCode;     // Stores generated assembly instructions
    map<string, string> registerMap; // Map TAC variables to registers
    int regCounter;                  // Counter for available registers

public:
    AssemblyGenerator() : regCounter(0) {}

    void generateFromTAC(const vector<TAC> &tacList)
    {
        for (const auto &tac : tacList)
        {
            // Ensure the result variable has a register allocated
            if (registerMap.find(tac.result) == registerMap.end())
            {
                registerMap[tac.result] = allocateRegister(); // Allocate a register for result
            }

            // Handle assignment
            if (tac.op == "=") // Assignment
            {
                if (isNumber(tac.arg1) || isBoolean(tac.arg1) || isString(tac.arg1))
                {
                    assemblyCode.push_back("MOV " + registerMap[tac.result] + ", " + tac.arg1);
                }
                else
                {
                    // Ensure arg1 is in a register
                    if (registerMap.find(tac.arg1) == registerMap.end())
                    {
                        registerMap[tac.arg1] = allocateRegister();
                        assemblyCode.push_back("MOV " + registerMap[tac.arg1] + ", " + tac.arg1);
                    }
                    assemblyCode.push_back("MOV " + registerMap[tac.result] + ", " + registerMap[tac.arg1]);
                }
            }
            // Handle binary operations
            else if (tac.op == "+" || tac.op == "-" || tac.op == "*" || tac.op == "/")
            {
                string opAssembly = (tac.op == "+") ? "ADD" : (tac.op == "-") ? "SUB"
                                                          : (tac.op == "*")   ? "MUL"
                                                                              : "DIV";
                // Ensure both operands are in registers
                if (registerMap.find(tac.arg1) == registerMap.end())
                {
                    registerMap[tac.arg1] = allocateRegister();
                    assemblyCode.push_back("MOV " + registerMap[tac.arg1] + ", " + tac.arg1);
                }
                if (registerMap.find(tac.arg2) == registerMap.end())
                {
                    registerMap[tac.arg2] = allocateRegister();
                    assemblyCode.push_back("MOV " + registerMap[tac.arg2] + ", " + tac.arg2);
                }
                assemblyCode.push_back(opAssembly + " " + registerMap[tac.arg1] + ", " + registerMap[tac.arg2]);
                assemblyCode.push_back("MOV " + registerMap[tac.result] + ", " + registerMap[tac.arg1]);
            }
            // Handle return statement
            else if (tac.op == "return")
            {
                if (registerMap.find(tac.arg1) != registerMap.end())
                {
                    assemblyCode.push_back("MOV EAX, " + registerMap[tac.arg1]);
                }
                else
                {
                    assemblyCode.push_back("MOV EAX, " + tac.arg1); // Assuming arg1 is a constant or immediate value
                }
                assemblyCode.push_back("RET");
            }
        }
    }

    void printAssembly()
    {
        cout << "Generated Assembly Code is:" << endl;
        for (const auto &line : assemblyCode)
        {
            cout << line << endl;
        }
    }

private:
    string allocateRegister()
    {
        return "R" + to_string(regCounter++); // Simple register allocation (R0, R1, ...)
    }

    bool isNumber(const string &s)
    {
        // Check if the string is a number (integer or float)
        return !s.empty() && (isdigit(s[0]) || s[0] == '-' || s[0] == '+');
    }

    bool isBoolean(const string &s)
    {
        return s == "true" || s == "false";
    }

    bool isString(const string &s)
    {
        return s.front() == '"' && s.back() == '"'; // Simple check for string literals
    }
};

class Lexer
{
private:
    string src;
    size_t pos;
    int lineNumber; // Line number tracker

public:
    Lexer(const string &src) : src(src), pos(0), lineNumber(1) {}

    vector<Token> tokenize()
    {
        vector<Token> tokens;
        while (pos < src.size())
        {
            char current = src[pos];

            if (current == '/' && src[pos + 1] == '/')
            {
                skipSingleLineComment();
                continue;
            }
            if (current == '/' && src[pos + 1] == '*')
            {
                skipMultiLineComment();
                continue;
            }
            if (isspace(current))
            {
                if (current == '\n')
                    lineNumber++;
                pos++;
                continue;
            }
            if (isdigit(current) || (current == '.' && isdigit(src[pos + 1])))
            {
                string number = consumeNumber();
                if (number.find('.') != string::npos)
                {
                    tokens.push_back(Token{T_FLOAT, number, lineNumber});
                }
                else
                {
                    tokens.push_back(Token{T_NUM, number, lineNumber});
                }
                continue;
            }
            if (isalpha(current))
            {
                string word = consumeWord();
                TokenType tokenType = identifyKeyword(word);
                tokens.push_back(Token{tokenType, word, lineNumber});
                continue;
            }
            if (current == '"')
            {
                tokens.push_back(Token{T_STRING, consumeString(), lineNumber});
                continue;
            }
            if (current == '\'')
            {
                tokens.push_back(Token{T_CHAR, consumeChar(), lineNumber});
                continue;
            }
            switch (current)
            {
            case '+':
                tokens.push_back(Token{TokenType::T_PLUS, "+", lineNumber});
                break;
            case '-':
                tokens.push_back(Token{TokenType::T_MINUS, "-", lineNumber});
                break;
            case '*':
                tokens.push_back(Token{TokenType::T_MUL, "*", lineNumber});
                break;
            case '/':
                tokens.push_back(Token{TokenType::T_DIV, "/", lineNumber});
                break;
            case '=':
                if (pos + 1 < src.size() && src[pos + 1] == '=')
                {
                    tokens.push_back(Token{TokenType::T_EQ, "==", lineNumber});
                    pos++;
                }
                else
                {
                    tokens.push_back(Token{TokenType::T_ASSIGN, "=", lineNumber});
                }
                break;
            case '<':
                if (pos + 1 < src.size() && src[pos + 1] == '=')
                {
                    tokens.push_back(Token{TokenType::T_LE, "<=", lineNumber});
                    pos++;
                }
                else
                {
                    tokens.push_back(Token{TokenType::T_LE, "<", lineNumber});
                }
                break;
            case '>':
                if (pos + 1 < src.size() && src[pos + 1] == '=')
                {
                    tokens.push_back(Token{TokenType::T_GE, ">=", lineNumber});
                    pos++;
                }
                else
                {

                    tokens.push_back(Token{TokenType::T_GT, ">", lineNumber});
                }
                break;
            case '(':
                tokens.push_back(Token{TokenType::T_LPAREN, "(", lineNumber});
                break;
            case ')':
                tokens.push_back(Token{TokenType::T_RPAREN, ")", lineNumber});
                break;
            case '{':
                tokens.push_back(Token{TokenType::T_LBRACE, "{", lineNumber});
                break;
            case '}':
                tokens.push_back(Token{TokenType::T_RBRACE, "}", lineNumber});
                break;
            case ';':
                tokens.push_back(Token{TokenType::T_SEMICOLON, ";", lineNumber});
                break;
            default:
                cout << "Unexpected character: " << current << endl;
                break;
            }
            pos++;
        }
        tokens.push_back(Token{T_EOF, "", lineNumber}); // EOF token
        return tokens;
    }

    void skipSingleLineComment()
    {
        while (pos < src.size() && src[pos] != '\n')
        {
            pos++;
        }
        lineNumber++; // Increase line number after skipping the comment
        pos++;        // Skip the newline character
    }

    string consumeNumber()
    {
        size_t start = pos;
        bool isFloat = false;

        while (pos < src.size() && (isdigit(src[pos]) || src[pos] == '.'))
        {
            if (src[pos] == '.')
            {
                if (isFloat) // If a second '.' is found, it's an error
                {
                    cout << "Error: Invalid number format at line " << lineNumber << endl;
                    exit(1);
                }
                isFloat = true;
            }
            pos++;
        }

        string number = src.substr(start, pos - start);
        if (isFloat)
        {
            return number; // Handle it as a float
        }
        return number; // Handle it as an integer
    }

    string consumeString()
    {
        pos++; // Skip the opening quote
        size_t start = pos;
        while (pos < src.size() && src[pos] != '"')
        {
            pos++;
        }
        if (pos >= src.size())
        {
            cout << "Error: Unterminated string at line " << lineNumber << endl;
            exit(1);
        }
        size_t length = pos - start;
        pos++; // Skip the closing quote
        return src.substr(start, length);
    }

    string consumeChar()
    {
        pos++; // Skip the opening single quote
        if (pos + 1 >= src.size() || src[pos + 1] != '\'')
        {
            cout << "Error: Invalid character literal at line " << lineNumber << endl;
            exit(1);
        }
        char c = src[pos];
        pos += 2; // Skip the character and closing single quote
        return string(1, c);
    }

    string consumeWord()
    {
        size_t start = pos;
        while (pos < src.size() && isalnum(src[pos]))
            pos++;
        return src.substr(start, pos - start);
    }

    void skipMultiLineComment()
    {
        pos += 2; // Skip the opening "/*"
        while (pos < src.size() - 1)
        {
            if (src[pos] == '*' && src[pos + 1] == '/')
            {
                pos += 2; // Skip the closing "*/"
                return;
            }
            if (src[pos] == '\n')
            {
                lineNumber++; // Track new lines inside the comment
            }
            pos++;
        }
        cout << "Error: Unterminated multi-line comment starting at line " << lineNumber << endl;
        exit(1); // Unterminated comment error
    }

    TokenType identifyKeyword(const string &inputWord)
    {
        string word = inputWord;
        for (size_t i = 0; i < word.length(); ++i)
        {
            word[i] = std::tolower(word[i]); // Apply tolower to each character
        }
        // cout << word << endl;
        if (word == "int")
        {
            return T_INT;
        }
        if (word == "float")
        {
            return T_FLOAT;
        }
        if (word == "char")
        {
            return T_CHAR;
        }
        if (word == "agar")
        {
            return T_IF;
        }
        if (word == "if")
        {
            return T_IF;
        }
        if (word == "else")
        {
            return T_ELSE;
        }
        if (word == "while")
        {
            return T_WHILE;
        }
        if (word == "for")
        {
            return T_FOR;
        }
        if (word == "void")
        {
            return T_VOID;
        }
        if (word == "return")
        {
            return T_RETURN;
        }
        if (word == "print")
        {
            return T_PRINT;
        }
        if (word == "string")
        {
            return T_STRING;
        }
        if (word == "if")
        {
            return T_IF;
        }
        if (word == "bool")
        {
            return T_BOOL;
        }
        if (word == "true" || word == "false")
        {
            return T_BOOL; // Return boolean token type
        }
        if (word == "function")
        {
            return T_FUNCTION; // Return function token type
        }

        return T_ID;
    }
};

class Parser
{
public:
    Parser(const vector<Token> &tokens, SymbolTable &symbolTable)
        : tokens(tokens), symbolTable(symbolTable), pos(0) {}

    void parseProgram()
    {
        while (tokens[pos].type != T_EOF)
        {
            // cout << tokens[pos].type << endl;
            parseStatement();
        }
        cout << "Parsing completed successfully! No Syntax Error" << endl;
        printTAC();
        symbolTable.printTable();
    }

    vector<TAC> getTACList() const
    {
        return tacList;
    }

    void printTokens()
    {
        for (int i = 0; i < tokens.size(); i++)
        {
            cout << "Type: " << tokenTypeToString(tokens[i].type)
                 << " Value: " << tokens[i].value
                 << " Line: " << tokens[i].line << endl;
        }
    }

private:
    vector<Token> tokens;
    size_t pos;
    SymbolTable &symbolTable;
    vector<TAC> tacList; // List to store generated TAC

    void parseStatement()
    {
        if (tokens[pos].type == T_INT || tokens[pos].type == T_FLOAT ||
            tokens[pos].type == T_CHAR || tokens[pos].type == T_STRING ||
            tokens[pos].type == T_BOOL)
        {
            parseDeclaration();
        }
        else if (tokens[pos].type == T_ID)
        {
            if (tokens[pos + 1].type == T_LPAREN) // Check for function call
            {
                parseFunctionCall(); // Implement this method to handle function calls
            }
            else
            {
                parseAssignment();
            }
        }
        else if (tokens[pos].type == T_IF)
        {
            // cout << "here is working";
            parseIfStatement();
            // cout << "passed";
        }
        else if (tokens[pos].type == T_RETURN)
        {
            parseReturnStatement();
        }
        else if (tokens[pos].type == T_WHILE)
        {
            parseWhileStatement();
        }
        else if (tokens[pos].type == T_FOR)
        {
            parseForStatement();
        }
        else if (tokens[pos].type == T_PRINT)
        {
            parsePrintStatement();
        }
        else if (tokens[pos].type == T_LBRACE)
        {
            parseBlock();
        }
        else if (tokens[pos].type == T_FUNCTION) // Check for function declaration
        {
            parseFunctionDeclaration();
        }
        else
        {
            cout << "here in parse statement" << endl;

            cout << "Unexpected token '" << tokens[pos].value
                 << "' on line " << tokens[pos].line << endl;
            exit(1);
        }
    }
    void parseFunctionCall()
    {
        string funcName = tokens[pos].value; // Store the function name
        expect(T_ID);                        // Expect the function name
        expect(T_LPAREN);                    // Expect '('

        // Parse arguments (optional)
        while (tokens[pos].type != T_RPAREN)
        {
            string arg = parseExpression(); // Parse each argument
            // You can generate TAC for function arguments if needed
            if (tokens[pos].type == T_COMMA)
            {
                expect(T_COMMA); // Expect ',' for next argument
            }
        }
        expect(T_RPAREN);                                 // Expect ')'
        expect(T_SEMICOLON);                              // Expect ';' after function call
        tacList.push_back(TAC("call", funcName, "", "")); // Generate TAC for function call
    }
    void parseDeclaration()
    {
        string type = tokens[pos].value; // Store the type
        // cout << tokens[pos].value << endl;
        // cout << tokens[pos].type << endl;
        expect(tokens[pos].type);      // Expect type (int, float, char, string)
        string id = tokens[pos].value; // Store the identifier
        expect(T_ID);
        // cout << type << id << endl;
        if (tokens[pos].type == T_ASSIGN)
        {
            expect(T_ASSIGN);
            string value = parseExpression();
            // cout << value << endl;
            tacList.push_back(TAC("=", value, "", id)); // Generate TAC for assignment
            symbolTable.addSymbol(id, type, tokens[pos].line);
        }
        else
        {
            symbolTable.addSymbol(id, type, tokens[pos].line); // Add to symbol table without initialization
        }

        expect(T_SEMICOLON);
    }

    void parseAssignment()
    {
        string id = tokens[pos].value; // Store the identifier
        // cout << id << endl;
        expect(T_ID);
        expect(T_ASSIGN);
        string result = parseExpression();
        // cout << result << endl;
        expect(T_SEMICOLON);
        tacList.push_back(TAC("=", result, "", id)); // Generate TAC for assignment
    }

    void parseIfStatement()
    {
        // cout << "here" << endl;
        expect(T_IF);
        // cout << "here1" << endl;
        expect(T_LPAREN);
        // cout << "here2" << endl;
        string condition = parseExpression(); // Parse the condition expression
        // cout << "here3" << endl;
        expect(T_RPAREN);
        parseStatement();
        if (tokens[pos].type == T_ELSE)
        {
            expect(T_ELSE);
            parseStatement();
        }
    }
    void parseFunctionDeclaration()
    {
        expect(T_FUNCTION);                  // Expect the 'function' keyword
        string funcName = tokens[pos].value; // Store the function name
        expect(T_ID);                        // Expect the function name
        expect(T_LPAREN);                    // Expect '('

        // Parse parameters (optional)
        while (tokens[pos].type != T_RPAREN)
        {
            string paramType;

            // Check for valid parameter types
            if (tokens[pos].type == T_INT || tokens[pos].type == T_FLOAT ||
                tokens[pos].type == T_CHAR || tokens[pos].type == T_STRING ||
                tokens[pos].type == T_BOOL) // Add T_BOOL here
            {
                paramType = tokens[pos].value; // Store parameter type
                expect(tokens[pos].type);      // Expect parameter type
            }
            else
            {
                cout << "Unexpected parameter type '" << tokens[pos].value
                     << "' on line " << tokens[pos].line << endl;
                exit(1);
            }

            string paramName = tokens[pos].value;                          // Store parameter name
            expect(T_ID);                                                  // Expect parameter name
            symbolTable.addSymbol(paramName, paramType, tokens[pos].line); // Add parameter to symbol table

            if (tokens[pos].type == T_COMMA)
            {
                expect(T_COMMA); // Expect ',' for next parameter
            }
        }
        expect(T_RPAREN); // Expect ')'
        expect(T_LBRACE); // Expect '{'

        // Parse function body
        while (tokens[pos].type != T_RBRACE)
        {
            parseStatement();
        }
        expect(T_RBRACE); // Expect '}'
    }

    void parseReturnStatement()
    {
        expect(T_RETURN);
        string result = parseExpression();
        expect(T_SEMICOLON);
        tacList.push_back(TAC("return", result, "", "")); // Generate TAC for return
    }

    void parseWhileStatement()
    {
        expect(T_WHILE);
        expect(T_LPAREN);
        string condition = parseExpression();
        expect(T_RPAREN);
        parseStatement();
    }

    void parsePrintStatement()
    {
        expect(T_PRINT);
        expect(T_LPAREN);
        string value = parseExpression();
        expect(T_RPAREN);
        expect(T_SEMICOLON);
        tacList.push_back(TAC("print", value, "", "")); // Generate TAC for print
    }

    void parseBlock()
    {
        expect(T_LBRACE);
        while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF)
        {
            parseStatement();
        }
        expect(T_RBRACE);
    }

    string parseExpression()
    {
        string result = parseTerm();
        // cout << result << endl;
        while (tokens[pos].type == T_PLUS || tokens[pos].type == T_MINUS ||
               tokens[pos].type == T_GT || tokens[pos].type == T_LT || tokens[pos].type == T_GE || tokens[pos].type == T_LE || tokens[pos].type == T_EQ)
        {
            string op = tokens[pos].value;
            pos++;
            string right = parseTerm();
            string tempVar = "t" + to_string(tacList.size());
            tacList.push_back(TAC(op, result, right, tempVar)); // Generate TAC for expression
            result = tempVar;
        }
        return result;
    }

    string parseTerm()
    {
        string result = parseFactor();
        while (tokens[pos].type == T_MUL || tokens[pos].type == T_DIV)
        {
            string op = tokens[pos].value;
            pos++;
            string right = parseFactor();
            string tempVar = "t" + to_string(tacList.size());
            tacList.push_back(TAC(op, result, right, tempVar));
            result = tempVar;
        }
        return result;
    }

    void parseForStatement()
    {
        expect(T_FOR);
        expect(T_LPAREN);

        // Parse initialization (optional)
        if (tokens[pos].type == T_INT || tokens[pos].type == T_FLOAT || tokens[pos].type == T_CHAR || tokens[pos].type == T_STRING || tokens[pos].type == T_BOOL)
        {
            parseDeclaration(); // Parse declaration, like `int i ;`
        }
        else if (tokens[pos].type == T_ID)
        {
            parseAssignment(); // Parse assignment, like `i = 0;`
        }
        else
        {
            expect(T_SEMICOLON);
        }
        string condition = parseExpression(); // Parse condition expression
        expect(T_SEMICOLON);
        if (tokens[pos].type == T_ID)
        {
            parseAssignment();
        }
        expect(T_RPAREN);
        parseStatement(); // Parse the body of the `for` loop
    }

    string parseFactor()
    {
        if (tokens[pos].type == T_NUM || tokens[pos].type == T_FLOAT || tokens[pos].type == T_ID || tokens[pos].type == T_BOOL)
        {
            string value = tokens[pos].value;
            pos++;
            return value;
        }
        else if (tokens[pos].type == T_CHAR || tokens[pos].type == T_STRING)
        {
            string value = tokens[pos].value;
            // cout << value << endl;
            pos++;
            return value;
        }
        else if (tokens[pos].type == T_LPAREN)
        {
            expect(T_LPAREN);
            string result = parseExpression();
            expect(T_RPAREN);
            return result;
        }
        else
        {
            cout << "here in parse factor" << endl;
            cout << "Unexpected token '" << tokens[pos].value
                 << "' on line " << tokens[pos].line << endl;
            exit(1);
        }
    }

    void expect(TokenType type)
    {
        if (tokens[pos].type == type)
        {
            pos++;
        }
        else
        {
            cout << "Expected " << tokenTypeToString(type)
                 << " but found '" << tokens[pos].value
                 << "' on line " << tokens[pos].line << endl;
            exit(1);
        }
    }

    void printTAC()
    {
        cout << "Three Address Code (TAC):" << endl;
        for (const auto &tac : tacList)
        {
            cout << tac.op << " " << tac.arg1 << ", " << tac.arg2 << " => " << tac.result << endl;
        }
    }
};

int main(int argc, char *argv[])
{
    if (argc < 2)
    {
        cout << "Usage: " << argv[0] << " <filename>" << endl;
        return 1;
    }

    ifstream file(argv[1]);
    if (!file)
    {
        cout << "Error: Cannot open file " << argv[1] << endl;
        return 1;
    }

    string input((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());

    Lexer lexer(input);
    vector<Token> tokens = lexer.tokenize();
    SymbolTable symbolTable;            // Declare a SymbolTable object
    Parser parser(tokens, symbolTable); // Pass symbolTable to the Parser constructor
    parser.printTokens();
    parser.parseProgram();

    // // Generate assembly code from TAC
    AssemblyGenerator asmGen;
    asmGen.generateFromTAC(parser.getTACList());
    asmGen.printAssembly();

    return 0;
}