// Test cases for escape analysis

function objectDoesNotEscape() {
    const obj = {};
    obj.x = 1;
    obj.y = 2;
    // Object doesn't escape - only used locally
    const sum = obj.x + obj.y;
    return sum;
}

function objectEscapesViaReturn() {
    const obj = {};
    obj.x = 1;
    obj.y = 2;
    return obj; // Object escapes via return
}

function objectEscapesViaFunctionCall() {
    const obj = {};
    obj.x = 1;
    console.log(obj); // Object escapes as function argument
    return 42;
}

function objectEscapesViaStorage() {
    const container = {};
    const obj = {};
    obj.x = 1;
    container.nested = obj; // Object escapes by being stored
    return container;
}