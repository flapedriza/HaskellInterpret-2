#header
<<
#include <string>
#include <iostream>
using namespace std;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr,int ttype, char *textt);
>>

<<
#include <cstdlib>
#include <cmath>

//global structures
//AST *root;


// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
  if (type == ID) {
    attr->kind = "id";
    attr->text = text;
  }
  else if(type == ASSIGN) {
    attr->kind = "Assign";
    attr->text = "";
  }
  else if(type == OR || type == AND || type == NOT) {
    attr->kind = text;
    attr->text = "";
  }
  else if(type == EQ) {
    attr->kind = "Eq";
    attr->text = "";
  }
  else if(type == GT) {
    attr->kind = "Gt";
    attr->text = "";
  }
  else if(type == PLUS) {
    attr->kind = "Plus";
    attr->text = "";
  }
  else if(type == MINUS) {
    attr->kind = "Minus";
    attr->text = "";
  }
  else if(type == TIMES) {
    attr->kind = "Times";
    attr->text = "";
  }
  else if(type == COND) {
    attr->kind = "Cond";
    attr->text = "";
  }
  else if(type == LOOP) {
    attr->kind = "Loop";
    attr->text = "";
  }
  else {
    for(int i = 1; text[i]; ++i) text[i] = tolower(text[i]);
    attr->kind = text;
    attr->text = "";
  }
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind; 
  as->text = attr->text;
  as->right = NULL; 
  as->down = NULL;
  return as;
}


/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
 AST *as=new AST;
 as->kind="Seq";
 as->right=NULL;
 as->down=child;
 return as;
}

AST* createASTvar(AST *child) {
  AST *as=new AST;
  as->kind="Var";
  as->right=NULL;
  as->down=child;
  return as;
}

AST* createASTconst(AST *child) {
  AST *as=new AST;
  as->kind="Const";
  as->right=NULL;
  as->down=child;
  return as;
}

/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a,int n) {
AST *c=a->down;
for (int i=0; c!=NULL && i<n; i++) c=c->right;
return c;
}



/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a,string s)
{
  if (a==NULL) return;

  cout<<a->kind;
  if (a->text!="") cout<<"("<<a->text<<")";
  cout<<endl;

  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    cout<<s+"  \\__";
    ASTPrintIndent(i,s+"  |"+string(i->kind.size()+i->text.size(),' '));
    i=i->right;
  }
  
  if (i!=NULL) {
      cout<<s+"  \\__";
      ASTPrintIndent(i,s+"   "+string(i->kind.size()+i->text.size(),' '));
      i=i->right;
  }
}

/// print AST 
void ASTPrint(AST *a)
{
  while (a!=NULL) {
    cout<<" ";
    ASTPrintIndent(a,"");
    a=a->right;
  }
}

void programPrint(AST* a);

void commandPrint(AST* a) {
  if(a->kind == "Seq") {
      cout << " (Seq [";
      a = a->down;
      commandPrint(a);
      a = a->right;
      while(a!=NULL) {
        cout << ",";
        commandPrint(a);
        a = a->right;
      }
      cout << "])";
    }
    else if(a->kind == "id") {
      cout << " \"" << a->text << "\" ";
    }
    else {
      cout << "(" << a->kind;
      a = a->down;
      programPrint(a);
      cout << ")";
    }
}

void programPrint(AST *a) {
  while(a != NULL) {
    commandPrint(a);
    a = a->right;
  }
}



int main() {
  AST *root = NULL;
  ANTLR(program(&root), stdin);
  //ASTPrint(root);
  programPrint(root);
}
>>

#lexclass START
#token NUM "[0-9]+"
#token ASSIGN ":="
#token INPUT "INPUT"
#token PRINT "PRINT"
#token EMPTY "EMPTY"
#token PUSH "PUSH"
#token POP "POP"
#token SIZE "SIZE"
#token COND "IF"
#token THEN "THEN"
#token ELSE "ELSE"
#token END "END"
#token LOOP "WHILE"
#token DO "DO"
#token GT "\>"
#token EQ "="
#token PLUS "\+"
#token MINUS "\-"
#token TIMES "\*"
#token AND "AND"
#token OR "OR"
#token NOT "NOT"
#token ID "[a-zA-Z0-9]+"
#token SPACE "[\ \n]" << zzskip();>>

program: instr "@"!;
instr: (instruction)* <<#0=createASTlist(_sibling);>>;
instruction: assign | input | print | empty | push | pop | size | cond | loop;
assign: ID ASSIGN^ numexpr;
input: INPUT^ ID;
print: PRINT^ ID;
empty: EMPTY^ ID;
push: PUSH^ ID numexpr;
pop: POP^ ID ID;
size: SIZE^ ID ID;
cond: COND^ boolexpr THEN! instr ELSE! instr END!;
loop: LOOP^ boolexpr DO! instr END!;
numexpr: term (PLUS^ term | MINUS^ term)*;
term: varconst (TIMES^ varconst)*;
var: ID <<#0=createASTvar(_sibling);>>;
consta: NUM <<#0=createASTconst(_sibling);>>;
varconst: var | consta;
boolexpr: boole (OR^ boole)*;
boole: (neg | comp) (AND^ (neg | comp))*;
neg: NOT^ comp;
comp: numexpr (GT^ numexpr | EQ^ numexpr );



