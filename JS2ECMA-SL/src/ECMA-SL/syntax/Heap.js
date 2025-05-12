const ValModule = require("./Val/Val");
const SymbolVal = ValModule.SymbolVal;

class Heap{

	constructor(){
		this.obj_counter = 0;
		this.heap =[];
	}

	createObject(){
		var obj_name = "_obj_"+ this.obj_counter;
		this.heap[obj_name]={};
		this.obj_counter++;
		return obj_name;
	}

	fieldCheck(object,field){
		var obj = this.heap[object];
		if(obj == undefined) return false;
			else {
				if (obj[field]== undefined) return false;
				else return true;
			}
	}
	getField(object,field){
		var result = this.heap[object][field];
		if(result === undefined){
			console.log(SymbolVal);
			return new SymbolVal("undefined");
		} else{
			return this.heap[object][field];
		}
	}
	getObject(object){
		return this.heap[object];
	}
	deleteObject(object){
		delete this.heap[object];
	}
	deleteField(object, field){
		delete this.heap[object][field];
	}
	setSecObj(object,field, exists_lvl, val_lvl){
		this.heap[object].sec_object[field] = {};
		this.heap[object].sec_object[field].exists_lvl = exists_lvl;
		this.heap[object].sec_object[field].val_lvl = val_lvl;
	}
	setFieldValue(object, field, val){
		this.heap[object][field] = val;	
	}

}

module.exports = Heap;
