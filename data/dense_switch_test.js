function denseSwitchTest(value) {
    switch (value) {
        case 0: return "zero";
        case 1: return "one";
        case 2: return "two";
        case 3: return "three";
        case 4: return "four";
        case 5: return "five";
        case 6: return "six";
        case 7: return "seven";
        case 8: return "eight";
        case 9: return "nine";
        case 10: return "ten";
        case 11: return "eleven";
        case 12: return "twelve";
        case 13: return "thirteen";
        case 14: return "fourteen";
        case 15: return "fifteen";
        case 16: return "sixteen";
        case 17: return "seventeen";
        case 18: return "eighteen";
        case 19: return "nineteen";
        case 20: return "twenty";
        default: return "other";
    }
}

// Test with negative numbers and larger ranges
function largeSwitchTest(x) {
    switch (x) {
        case -5: return "negative five";
        case -4: return "negative four";
        case -3: return "negative three";
        case -2: return "negative two";
        case -1: return "negative one";
        case 0: return "zero";
        case 1: return "one";
        case 2: return "two";
        case 3: return "three";
        case 4: return "four";
        case 5: return "five";
        case 6: return "six";
        case 7: return "seven";
        case 8: return "eight";
        case 9: return "nine";
        case 10: return "ten";
        default: return "unknown";
    }
}

// Test switch with break statements instead of return
function switchWithBreak(value) {
    let result = "default";
    switch (value) {
        case 0:
            result = "zero";
            break;
        case 1:
            result = "one";
            break;
        case 2:
            result = "two";
            break;
        case 3:
            result = "three";
            break;
        case 4:
            result = "four";
            break;
        case 5:
            result = "five";
            break;
        default:
            result = "other";
            break;
    }
    return result;
}

// Test switch with fallthrough cases
function switchWithFallthrough(value) {
    let result = "";
    switch (value) {
        case 0:
        case 1:
        case 2:
            result += "low ";
            // fallthrough
        case 3:
        case 4:
            result += "mid ";
            break;
        case 5:
        case 6:
        case 7:
            result += "high";
            break;
        default:
            result = "unknown";
    }
    return result;
}

// Test simple nested switch
function nestedSwitch(outer, inner) {
    switch (outer) {
        case 0:
            switch (inner) {
                case 0: return "0-0";
                case 1: return "0-1";
                case 2: return "0-2";
                default: return "0-other";
            }
        case 1:
            switch (inner) {
                case 0: return "1-0";
                case 1: return "1-1";
                case 2: return "1-2";
                default: return "1-other";
            }
        case 2:
            return "outer-2";
        default:
            switch (inner) {
                case 0: return "default-0";
                case 1: return "default-1";
                default: return "default-other";
            }
    }
}

// Test switch with mixed break/return
function mixedBreakReturn(value) {
    let prefix = "value: ";
    switch (value) {
        case 0:
            return "immediate zero";
        case 1:
            prefix = "special: ";
            break;
        case 2:
            prefix = "double: ";
            // fallthrough
        case 3:
            return prefix + "two or three";
        case 4:
            prefix = "quad: ";
            break;
        default:
            return "unknown";
    }
    return prefix + value;
}

// Test 1: Switch with function calls and side effects
function switchWithFunctionCalls(mode, value) {
    let result = 0;
    
    function helper(x) {
        return x * 2;
    }
    
    function sideEffect(x) {
        result += x;
        return x + 1;
    }
    
    switch (mode) {
        case 0:
            result = helper(value);
            break;
        case 1:
            result = sideEffect(value);
            switch (result) {
                case 1: return "one after side effect";
                case 2: return "two after side effect";
                default: return "other: " + result;
            }
        case 2:
            // Instead of loop, unroll it
            result += helper(0);
            result += helper(1);
            result += helper(2);
            break;
        case 3:
            // Instead of while loop, do a few iterations
            result = value;
            if (result < 10) {
                result = sideEffect(result);
                if (result < 10) {
                    result = sideEffect(result);
                    if (result < 10) {
                        result = sideEffect(result);
                    }
                }
            }
            break;
        default:
            return helper(sideEffect(value));
    }
    return result;
}

// Test 2: Complex nested control flow with switches (no loops)
function complexControlFlow(a, b, c) {
    let result = "";
    
    if (a > 0) {
        switch (b) {
            case 0:
                if (c === 0) {
                    return "early exit";
                }
                result = "a>0,b=0";
                break;
            case 1:
                // Instead of loop, handle specific cases
                switch (c) {
                    case 0: result = "no iterations"; break;
                    case 1: result = "first,"; break;
                    case 2: result = "first,second,"; break;
                    case 3: result = "first,second,other,"; break;
                    default: result = "many iterations";
                }
                break;
            default:
                result = "a>0,b=other";
        }
    } else {
        switch (c) {
            case 0:
            case 1:
                result = "a<=0,c=0or1";
                break;
            case 2:
                switch (b) {
                    case 0: return "nested: b=0,c=2";
                    case 1: return "nested: b=1,c=2";
                    default: 
                        result = "nested: b=other,c=2";
                        break;
                }
                break;
            default:
                if (b > 0) {
                    result = "complex default";
                } else {
                    return "early default exit";
                }
        }
    }
    
    return result;
}

// Test 3: Switch with try-catch and exception handling
function switchWithTryCatch(operation, value) {
    let result = "none";
    
    try {
        switch (operation) {
            case "divide":
                if (value === 0) throw new Error("Division by zero");
                result = 100 / value;
                break;
            case "sqrt":
                if (value < 0) throw new Error("Negative sqrt");
                result = Math.sqrt(value);
                break;
            case "parse":
                result = JSON.parse(value);
                break;
            case "nested":
                try {
                    switch (value) {
                        case 0: throw new Error("Zero error");
                        case 1: result = "one"; break;
                        default: result = "parsed";
                    }
                } catch (innerError) {
                    result = "inner catch: " + innerError.message;
                }
                break;
            default:
                result = "unknown operation";
        }
    } catch (e) {
        switch (e.message) {
            case "Division by zero":
                return "caught division error";
            case "Negative sqrt":
                return "caught sqrt error";
            default:
                return "caught: " + e.message;
        }
    }
    
    return result;
}

// Test 4: Switch with array/object operations and destructuring
function switchWithObjects(type, data) {
    let result;
    
    switch (type) {
        case "array":
            const arr = [1, 2, 3, 4, 5];
            switch (data) {
                case 0:
                    result = arr.map(x => x * 2);
                    break;
                case 1:
                    result = arr.filter(x => x % 2 === 0);
                    break;
                case 2:
                    const [first, ...rest] = arr;
                    result = { first, rest };
                    break;
                default:
                    result = arr.reduce((a, b) => a + b, 0);
            }
            break;
        case "object":
            const obj = { a: 1, b: 2, c: 3 };
            switch (data) {
                case 0:
                    result = Object.keys(obj);
                    break;
                case 1:
                    result = Object.values(obj);
                    break;
                case 2:
                    const { a, ...others } = obj;
                    result = { extracted: a, others };
                    break;
                default:
                    result = { ...obj, d: 4 };
            }
            break;
        case "mixed":
            const items = [{ id: 1, value: "a" }, { id: 2, value: "b" }];
            switch (data) {
                case 0:
                    result = items.find(item => item.id === 1);
                    break;
                case 1:
                    result = items.map(({ id, value }) => ({ 
                        newId: id * 10, 
                        newValue: value.toUpperCase() 
                    }));
                    break;
                default:
                    result = items.every(item => item.id > 0);
            }
            break;
        default:
            result = null;
    }
    
    return result;
}

// Test 5: Switch with async patterns (using callbacks to simulate)
function switchWithCallbacks(action, value, callback) {
    let internalState = 0;
    
    function processAsync(val, cb) {
        setTimeout(() => cb(val * 2), 0);
    }
    
    switch (action) {
        case "immediate":
            callback(value);
            break;
        case "delayed":
            processAsync(value, (result) => {
                switch (result) {
                    case 0: callback("zero result"); break;
                    case 2: callback("two result"); break;
                    case 4: callback("four result"); break;
                    default: callback("other: " + result);
                }
            });
            break;
        case "chain":
            processAsync(value, (first) => {
                internalState = first;
                switch (first) {
                    case 0:
                        callback("chain ended at zero");
                        break;
                    case 2:
                        processAsync(first, (second) => {
                            callback("chain result: " + second);
                        });
                        break;
                    default:
                        callback("chain default: " + first);
                }
            });
            break;
        case "conditional":
            if (value > 0) {
                processAsync(value, callback);
            } else {
                switch (value) {
                    case 0: callback("sync zero"); break;
                    case -1: callback("sync negative one"); break;
                    default: callback("sync negative");
                }
            }
            break;
        default:
            callback("unknown action");
    }
    
    return internalState;
}

// Test function calls
denseSwitchTest(15);
largeSwitchTest(7);
switchWithBreak(3);
switchWithFallthrough(0);
switchWithFallthrough(3);
switchWithFallthrough(6);
nestedSwitch(0, 1);
nestedSwitch(1, 3);
nestedSwitch(3, 0);
mixedBreakReturn(1);
mixedBreakReturn(2);
mixedBreakReturn(4);

// New test calls
switchWithFunctionCalls(0, 5);
switchWithFunctionCalls(1, 1);
switchWithFunctionCalls(2, 0);
complexControlFlow(1, 0, 1);
complexControlFlow(0, 1, 2);
complexControlFlow(-1, -1, 3);
switchWithTryCatch("divide", 5);
switchWithTryCatch("sqrt", -1);
switchWithTryCatch("nested", 0);
switchWithObjects("array", 1);
switchWithObjects("object", 2);
switchWithObjects("mixed", 0);
switchWithCallbacks("immediate", 10, console.log);
switchWithCallbacks("delayed", 2, console.log);
switchWithCallbacks("chain", 1, console.log);
