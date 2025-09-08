function testSimpleObject() {
    const obj = {};
    obj.x = 1;
    obj.y = 2;
    obj.z = 3;
    return obj;
}

function testArrayMutations() {
    const arr = [];
    arr[0] = "hello";
    arr[1] = "world";
    return arr;
}