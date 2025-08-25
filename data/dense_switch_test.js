// Dense switch test - basic numeric switch
function denseSwitchTest(n) {
    switch (n) {
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

// Large sparse switch
function largeSwitchTest(code) {
    switch (code) {
        case 100: return "continue";
        case 200: return "ok";
        case 201: return "created";
        case 400: return "bad request";
        case 401: return "unauthorized";
        case 403: return "forbidden";
        case 404: return "not found";
        case 500: return "server error";
        default: return "unknown";
    }
}

// Switch with breaks
function switchWithBreak(x) {
    let result = "";
    switch (x) {
        case 1:
            result += "one ";
            break;
        case 2:
            result += "two ";
            break;
        case 3:
            result += "three ";
            break;
        default:
            result += "other ";
            break;
    }
    result += "done";
    return result;
}

// Switch with fallthroughs
function switchWithFallthrough(x) {
    let result = "";
    switch (x) {
        case 0:
        case 1:
        case 2:
            result += "low ";
        case 3:
        case 4:
            result += "mid ";
            break;
        case 5:
        case 6:
        case 7:
            result = "high";
            break;
        default:
            result = "unknown";
    }
    return result;
}

// Nested switches
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

// Mixed break/return
function mixedBreakReturn(x) {
    let result = "";
    switch (x) {
        case 1:
            result = "one";
            break;
        case 2:
            return "two";
        case 3:
            result = "three";
            break;
        case 4:
            return "four";
        default:
            result = "default";
    }
    return result + "-processed";
}

// Switch with function calls
function switchWithFunctionCalls(op, a, b) {
    const add = (x, y) => x + y;
    const sub = (x, y) => x - y;
    const mul = (x, y) => x * y;
    const div = (x, y) => x / y;
    
    switch (op) {
        case 0: return add(a, b);
        case 1: return sub(a, b);
        case 2: return mul(a, b);
        case 3: return div(a, b);
        default: return 0;
    }
}

// Complex control flow
function complexControlFlow(a, b, c) {
    switch (a) {
        case 0:
            if (b > 0) {
                switch (c) {
                    case 0: return "a=0,b>0,c=0";
                    case 1: return "a=0,b>0,c=1";
                    default: return "a=0,b>0,c=other";
                }
            } else {
                return "a=0,b<=0";
            }
        case 1:
            if (b === 0) {
                return "a=1,b=0";
            } else if (b === 1) {
                return "a=1,b=1";
            } else {
                switch (c) {
                    case 2: return "a=1,b=other,c=2";
                    default: return "a=1,b=other,c=other";
                }
            }
        default:
            return "a=other";
    }
}

// Switch with try-catch
function switchWithTryCatch(operation, value) {
    let result;
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
            case "log":
                if (value <= 0) throw new Error("Invalid log");
                result = Math.log(value);
                break;
            default:
                result = value;
        }
        return result;
    } catch (e) {
        return "Error: " + e.message;
    }
}

// Switch with objects (no for-of loop)
function switchWithObjects(type, num) {
    switch (type) {
        case "array":
            const arr = [1, 2, 3, 4, 5];
            switch (num) {
                case 0:
                    return arr.map(x => x * 2);
                case 1:
                    return arr.filter(x => x % 2);
                case 2:
                    // Array slice case
                    return { first: arr[0], rest: arr.slice(1) };
                default:
                    return arr.reduce((a, b) => a + b, 0);
            }
        case "object":
            const obj = { a: 1, b: 2, c: 3 };
            switch (num) {
                case 0:
                    return Object.keys(obj);
                case 1:
                    return Object.values(obj);
                case 2:
                    const { a, ...others } = obj;
                    return { extracted: a, others };
                default:
                    return { ...obj, d: 4 };
            }
        case "mixed":
            const items = [
                { id: 1, value: "a" },
                { id: 2, value: "b" }
            ];
            switch (num) {
                case 0:
                    return items.find(x => x.id === 1);
                case 1:
                    return items.map(x => x.value);
                default:
                    return items.every(x => x.id > 0);
            }
        default:
            return null;
    }
}

// Test all functions
console.log(denseSwitchTest(15));
console.log(largeSwitchTest(404));
console.log(switchWithBreak(2));
console.log(switchWithFallthrough(1));
console.log(switchWithFallthrough(4));
console.log(switchWithFallthrough(6));
console.log(nestedSwitch(0, 1));
console.log(nestedSwitch(1, 2));
console.log(nestedSwitch(2, 0));
console.log(mixedBreakReturn(1));
console.log(mixedBreakReturn(2));
console.log(mixedBreakReturn(4));
console.log(switchWithFunctionCalls(0, 5, 3));
console.log(switchWithFunctionCalls(1, 5, 3));
console.log(switchWithFunctionCalls(2, 5, 3));
console.log(complexControlFlow(0, 1, 0));
console.log(complexControlFlow(1, 0, 0));
console.log(complexControlFlow(1, 2, 2));
console.log(switchWithTryCatch("divide", 0));
console.log(switchWithTryCatch("sqrt", -4));
console.log(switchWithObjects("array", 0));
console.log(switchWithObjects("mixed", 1));