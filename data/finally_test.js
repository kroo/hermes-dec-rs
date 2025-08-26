// Simple finally without catch
function justFinally() {
    try {
        console.log("Try");
        return 1;
    } finally {
        console.log("Finally");
    }
}

// Try-catch-finally
function tryCatchFinally() {
    try {
        console.log("Try");
        throw new Error("Test");
    } catch (e) {
        console.log("Catch:", e.message);
        return 2;
    } finally {
        console.log("Finally");
    }
}

// Finally with no catch and exception
function finallyWithException() {
    try {
        console.log("Try");
        throw new Error("Uncaught");
    } finally {
        console.log("Finally cleanup");
    }
}