  class Val{
  constructor(value) {
    this.value = value;
  }

  toString() {
    return this.value.toString();
  }
}

Val.Str = class {
  constructor(value) {
    this.value = value;
  }

  toString() {
    if (this.value.startsWith('"')) {
      return `"${this.value.split('"').join('\\"')}"`;
    }
    return `"${this.value}"`;
  }
};

Val.Flt = class {
  constructor(value) {
    this.value = value;
  }

  toString() {
    return this.value;
  }
};

Val.Int = class {
  constructor(value) {
    this.value = value;
  }

  toString() {
    return this.value;
  }
};

Val.Bool = class {
  constructor(value) {  
    this.value = value;
  }

  toString() {
    return this.value;
  }
};

Val.Null = class {
  toString() {
    return null;
  }
};

Val.fromJSON = function(obj){
  val_value = obj.value;
  value = val_value.value;
  return new Val(value);
}

module.exports = Val;
