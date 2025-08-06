// Test method vs function patterns

// Regular standalone function
function standalone() {
    return 42;
}

// Method defined in object literal
var obj = {
    method: function() {
        return this.value;
    },
    value: 100
};

// Method assigned to prototype
function MyClass() {
    this.x = 10;
}
MyClass.prototype.getInstance = function() {
    return this.x;
};

// Method assigned directly to object
var myObj = {};
myObj.doSomething = function() {
    return "doing something";
};

// IIFE (not a method)
(function() {
    console.log("IIFE");
})();

// Callback function (not a method but passed as argument)
setTimeout(function() {
    console.log("timeout");
}, 1000);