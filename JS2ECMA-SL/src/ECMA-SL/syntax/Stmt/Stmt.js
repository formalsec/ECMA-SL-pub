class Stmt {
  
}

const Print = require("./Print")(Stmt);
const Assign = require("./Assign")(Stmt);
const Condition = require("./Condition")(Stmt);
const Block = require("./Block")(Stmt);
const Loop = require("./Loop")(Stmt);
const Return = require("./Return")(Stmt);
const FieldAssign = require("./FieldAssign")(Stmt);
const FieldDelete = require("./FieldDelete")(Stmt);
const AssignCall = require("./AssignCall")(Stmt);
const AssignNewObj = require("./AssignNewObj")(Stmt);
const FieldLookup = require("./FieldLookup")(Stmt);
const AssignInObjCheck = require("./AssignInObjCheck")(Stmt);
const AssignObjToList = require("./AssignObjToList")(Stmt);
const Exception = require("./Exception")(Stmt);


Stmt.fromJSON = function(obj) {
   switch (obj.type) {
       case "skip": return new Skip(); 
       case "print": return Print.fromJSON(obj); 
       case "assign": return Assign.fromJSON(obj);
       case "condition": return Condition.fromJSON(obj);
       case "block": return Block.fromJSON(obj);//Leitura de Bloco
       case "loop": return Loop.fromJSON(obj);
       case "return": return Return.fromJSON(obj);
       case "fieldassign": return FieldAssign.fromJSON(obj);
       case "fielddelete": return FieldDelete.fromJSON(obj);
       case "assigncall": return AssignCall.fromJSON(obj);
       case "assignnewobject": return AssignNewObj.fromJSON(obj);
       case "fieldlookup": return FieldLookup.fromJSON(obj);
       case "assigninobjcheck": return AssignInObjCheck.fromJSON(obj);
       case "assignobjtolist": return AssignObjToList.fromJSON(obj);
       case "exception": return Exception.fromJSON(obj);
       default: throw new Error("Unsupported statement: "+obj.type); 
   }
}

 module.exports = Stmt