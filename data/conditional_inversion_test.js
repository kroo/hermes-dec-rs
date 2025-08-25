// Test cases for conditional inversion
// These patterns should be inverted when the if-branch is empty

function simpleInversion(x) {
    if (x > 10) {
        // empty
    } else {
        return "small";
    }
    return "default";
}

function guardClause(value) {
    if (value == null) {
        // empty - guard clause pattern
    } else {
        console.log("Processing:", value);
        return value * 2;
    }
    return 0;
}

function nestedInversion(a, b) {
    if (a > 0) {
        if (b > 0) {
            // empty
        } else {
            console.log("b is negative");
            return a - b;
        }
    } else {
        return -a;
    }
    return 0;
}

// Call the functions
simpleInversion(5);
simpleInversion(15);
guardClause(null);
guardClause(42);
nestedInversion(5, -3);
nestedInversion(-2, 4);