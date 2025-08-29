// Test file for try-catch exception handling
function simpleTryCatch() {
    try {
        console.log("In try block");
        throw new Error("Test error");
    } catch (e) {
        console.log("Caught error:", e.message);
    }
}

function tryWithFinally() {
    try {
        console.log("Try block");
        return 1;
    } catch (e) {
        console.log("Catch block");
        return 2;
    } finally {
        console.log("Finally block");
    }
}

function nestedTryCatch() {
    try {
        console.log("Outer try");
        try {
            console.log("Inner try");
            throw new Error("Inner error");
        } catch (innerErr) {
            console.log("Inner catch:", innerErr.message);
            throw new Error("Rethrow");
        }
    } catch (outerErr) {
        console.log("Outer catch:", outerErr.message);
    }
}

function tryInLoop() {
    for (let i = 0; i < 3; i++) {
        try {
            if (i === 1) {
                throw new Error("Loop error");
            }
            console.log("Loop iteration:", i);
        } catch (e) {
            console.log("Caught in loop:", e.message);
        }
    }
}

function multipleCatchVariables() {
    try {
        throw new Error("First error");
    } catch (err1) {
        console.log("First catch:", err1.message);
        try {
            throw new Error("Second error");
        } catch (err2) {
            console.log("Second catch:", err2.message);
        }
    }
}