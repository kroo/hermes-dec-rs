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
