#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

/* Como mejorar el SAT-solver:
 * - Cambiar la heurística de decisión: hay que decidir sobre qué variable y con qué signo
 *      -> Escogeré el literal que más veces se repita.
 * - Hacer la propagación más eficiente -> Jugar con la actividad de un literal
 */

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;
vector<int> modelStack;
uint indexOfNextLitToPropagate;
uint decisionLevel;


/* ******************************************************************************************
 * ***************************************** SAT-ANA ***************************************
 * ******************************************************************************************
*/

// Vector de vectors pels literals:
// Vector[numLiteral] = Vector de les posicions on es troben aquests literals.
vector<vector<int> > positiveLiterals;
vector<vector<int> > negativeLiterals;

vector<double> literalsActivity;

const int INCREMENT = 1.0;
const int TIME_TO_UPDATE = 1000;
const int DECREASE = 2.0;

int conflicts;
int decs;


void initialize(){

    conflicts = 0;
    decs = 0;

    positiveLiterals.resize(numVars + 1);
    negativeLiterals.resize(numVars + 1);
    literalsActivity.resize(numVars + 1, 0.0);
}


// The activity vector has to be scaled from time to time.
void itsTimeToUpdateTheActivity(){
    for(uint i = 1; i < numVars; ++i){
        literalsActivity[i] /= DECREASE;
    }
}


// When a literal gives conflict, we update its activity.
void updateLiteralActivity(int index){
    vector<int> literalClauses = clauses[index];
    int size = literalClauses.size();
    
    for(int i = 0; i < size; ++i){
        int lit = literalClauses[i];
        if(lit < 0) lit = -lit;
        literalsActivity[lit] += INCREMENT;
    }        
}



void activityHeuristic(int index){
    
    if(conflicts%TIME_TO_UPDATE == 0)
        itsTimeToUpdateTheActivity();
    
    updateLiteralActivity(index);
}


// Get next literal with an activity heuristic
int getNextLiteralWithSuperHeuristic(){
    int superTop = 0;
    int topness = 0;

    for(uint i = 1; i <= numVars; ++i){
        if(model[i] == UNDEF){
            if(literalsActivity[i] > topness){
                topness = literalsActivity[i];
                superTop = i;
            }
        }
    }
    ++decs;
    return superTop;
}


void readClauses(){

  // Skip comments
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  

  // Read "cnf numVars numClauses"
  string aux;
  cin >> aux >> numVars >> numClauses;
  clauses.resize(numClauses);
  initialize();

  // Read clauses
  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0) {
        clauses[i].push_back(lit);
        if(lit > 0) {
            positiveLiterals[lit].push_back(i);
            literalsActivity[lit] += INCREMENT;
        }
        else {
            negativeLiterals[-lit].push_back(i);
            literalsActivity[-lit] += INCREMENT;
        }
    }
  }    
}


int currentValueInModel(int lit){
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}


void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;		
}


bool propagateGivesConflict(){

  while (indexOfNextLitToPropagate < modelStack.size()) {
    vector<int>* vec;
    int lit = modelStack[indexOfNextLitToPropagate];
    if(lit <= 0) vec = &positiveLiterals[-lit];
    else vec = &negativeLiterals[lit];

    ++indexOfNextLitToPropagate;

    for (uint i = 0; i < vec->size(); ++i) {
      bool someLitTrue = false;
      int numUndefs = 0;
      int lastLitUndef = 0;

      int point = (*vec)[i];

        for (uint k = 0; not someLitTrue and k < clauses[point].size(); ++k){
            int val = currentValueInModel(clauses[point][k]);
            if (val == TRUE) someLitTrue = true;
            else if (val == UNDEF){
                ++numUndefs; 
                lastLitUndef = clauses[point][k];
            }
        }

        if (not someLitTrue and numUndefs == 0) {
            ++conflicts;
            activityHeuristic(point);
            return true;
        }
        
        else if (not someLitTrue and numUndefs == 1) 
            setLiteralToTrue(lastLitUndef);	
    }    
  }
  return false;
}

void backtrack(){
  uint i = modelStack.size() -1;
  int lit = 0;
  while (modelStack[i] != 0){ // 0 is the DL mark
    lit = modelStack[i];
    model[abs(lit)] = UNDEF;
    modelStack.pop_back();
    --i;
  }
  // at this point, lit is the last decision
  modelStack.pop_back(); // remove the DL mark
  --decisionLevel;
  indexOfNextLitToPropagate = modelStack.size();
  setLiteralToTrue(-lit);  // reverse last decision
}


void checkmodel(){
  for (uint i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);

    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:";
      
      for (uint j = 0; j < clauses[i].size(); ++j)
	cout << clauses[i][j] << " ";

      cout << endl;
      exit(1);
    }
  }  
}


int main(){ 
  readClauses(); // reads numVars, numClauses and clauses
  model.resize(numVars+1, UNDEF);
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  
  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {
          cout << "UNSATISFIABLE" << endl;
          cout << decs << " decisions" << endl;
          return 10;
      }
      else if (val == UNDEF) setLiteralToTrue(lit);
    }
  
  // DPLL algorithm
  while (true) {
    while (propagateGivesConflict()) {
      if (decisionLevel == 0) { 
          cout << "UNSATISFIABLE" << endl; 
          cout << decs << " decisions" << endl;
          return 10; 
      }
      backtrack();
    }

    int decisionLit = getNextLiteralWithSuperHeuristic();
    if (decisionLit == 0) { 
        checkmodel(); 
        cout << "SATISFIABLE" << endl; 
        cout << decs << " decisions" << endl;
        return 20; 
        
    }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel;
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}  
