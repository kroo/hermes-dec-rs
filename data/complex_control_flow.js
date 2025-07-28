// Complex control flow test cases for decompiler testing
// Focus: nested loops, breaks, continues, complex conditionals

function nestedLoopsWithBreakContinue(matrix) {
  let total = 0;
  let found = false;
  
  outer: for (let i = 0; i < matrix.length; i++) {
    for (let j = 0; j < matrix[i].length; j++) {
      if (matrix[i][j] < 0) {
        continue;
      }
      if (matrix[i][j] > 100) {
        found = true;
        break outer;
      }
      total += matrix[i][j];
      
      for (let k = 0; k < 3; k++) {
        if (k === 1 && total > 50) {
          continue;
        }
        total += k;
      }
    }
  }
  
  return found ? -1 : total;
}

function complexNestedConditionals(a, b, c, d) {
  if (a > 0) {
    if (b > 0) {
      if (c > 0) {
        if (d > 0) {
          return a + b + c + d;
        } else {
          return a + b + c - d;
        }
      } else if (c < -10) {
        return a + b - c;
      } else {
        if (d % 2 === 0) {
          return a + b;
        } else {
          return a - b;
        }
      }
    } else if (b < -5) {
      if (c > d) {
        return a - b + c;
      } else {
        return a - b - c;
      }
    } else {
      return a;
    }
  } else if (a < -10) {
    if (b > c && c > d) {
      return -a + b - c + d;
    } else {
      return -a;
    }
  } else {
    return 0;
  }
}

function multipleLoopTypes(arr) {
  let result = [];
  let i = 0;
  
  // While loop with complex condition
  while (i < arr.length && result.length < 10) {
    if (arr[i] % 2 === 0) {
      i++;
      continue;
    }
    
    // Nested for loop
    for (let j = i; j < Math.min(i + 3, arr.length); j++) {
      if (arr[j] > 50) {
        break;
      }
      result.push(arr[j]);
    }
    
    i++;
  }
  
  // Do-while loop
  let sum = 0;
  let idx = 0;
  do {
    if (idx < result.length) {
      sum += result[idx];
    }
    idx++;
  } while (idx < 5 && sum < 100);
  
  return {result, sum};
}

function switchWithFallthrough(value, mode) {
  let output = "";
  
  switch (mode) {
    case 1:
      output += "mode1-";
      // fallthrough
    case 2:
      output += "mode2-";
      if (value > 10) {
        output += "high-";
        break;
      }
      // fallthrough
    case 3:
      output += "mode3-";
      switch (value % 3) {
        case 0:
          output += "zero";
          break;
        case 1:
          output += "one";
          break;
        default:
          output += "two";
      }
      break;
    case 4:
      for (let i = 0; i < value; i++) {
        if (i > 5) break;
        output += i;
      }
      break;
    default:
      output = "unknown";
  }
  
  return output;
}

function complexLoopWithMultipleExits(data) {
  let processed = 0;
  let errors = 0;
  
  mainLoop: for (let category of data) {
    if (!category || !category.items) {
      errors++;
      continue;
    }
    
    for (let item of category.items) {
      if (item.skip) {
        continue;
      }
      
      if (item.critical_error) {
        errors++;
        if (errors > 3) {
          break mainLoop;
        }
        continue;
      }
      
      // Nested processing loop
      for (let step = 0; step < 5; step++) {
        if (step === 2 && item.fast_track) {
          step = 4; // Skip to end
          continue;
        }
        
        if (item.value && item.value[step]) {
          processed++;
          if (processed > 100) {
            return {status: "limit_reached", processed, errors};
          }
        }
        
        if (step === 3 && item.early_exit) {
          break; // Exit inner loop only
        }
      }
    }
    
    if (category.stop_after) {
      break;
    }
  }
  
  return {status: "completed", processed, errors};
}

function ternaryAndShortCircuit(x, y, z) {
  // Complex ternary expressions
  let result1 = x > 0 ? (y > 0 ? (z > 0 ? x + y + z : x + y - z) : x - y) : (y > 0 ? -x + y : -x - y);
  
  // Short circuit evaluation
  let result2 = x && y && z || x && y || x || 0;
  
  // Mixed with conditionals
  if (result1 > result2) {
    return x > y ? (z > x ? "case1" : "case2") : (z > y ? "case3" : "case4");
  } else if (result1 < result2) {
    return result1 && result2 ? "both_truthy" : (!result1 && !result2 ? "both_falsy" : "mixed");
  } else {
    return "equal";
  }
}

function exceptionHandlingControlFlow(operations) {
  let completed = [];
  let failed = [];
  
  for (let i = 0; i < operations.length; i++) {
    try {
      let op = operations[i];
      
      if (op.type === "divide") {
        if (op.b === 0) {
          throw new Error("Division by zero");
        }
        completed.push(op.a / op.b);
      } else if (op.type === "process") {
        for (let j = 0; j < op.steps; j++) {
          try {
            if (j === 2 && op.fail_at_step_2) {
              throw new Error("Step 2 failed");
            }
            if (j > 5) break;
          } catch (stepError) {
            if (op.continue_on_error) {
              continue;
            } else {
              throw stepError;
            }
          }
        }
        completed.push("processed");
      } else {
        throw new Error("Unknown operation");
      }
    } catch (error) {
      failed.push({index: i, error: error.message});
      
      if (operations[i].critical) {
        break; // Stop processing on critical failure
      }
      
      continue; // Skip to next operation
    } finally {
      // Cleanup that runs regardless
      if (operations[i] && operations[i].cleanup) {
        // This always runs
        try {
          // Even cleanup can fail
          if (Math.random() > 0.9) {
            throw new Error("Cleanup failed");
          }
        } catch (cleanupError) {
          // Handle cleanup errors
          failed.push({index: i, error: "cleanup: " + cleanupError.message});
        }
      }
    }
  }
  
  return {completed, failed};
}

function recursiveWithControlFlow(n, depth, options) {
  if (depth > 10) {
    return "max_depth";
  }
  
  if (n <= 1) {
    return 1;
  }
  
  let result = 0;
  
  for (let i = 1; i <= n; i++) {
    if (options.skip_even && i % 2 === 0) {
      continue;
    }
    
    if (options.early_exit && i > n / 2) {
      break;
    }
    
    try {
      let subResult = recursiveWithControlFlow(i - 1, depth + 1, options);
      
      if (typeof subResult === "string") {
        return subResult; // Propagate error
      }
      
      result += subResult;
      
      if (options.limit && result > options.limit) {
        return "limit_exceeded";
      }
    } catch (e) {
      if (options.ignore_errors) {
        continue;
      } else {
        throw e;
      }
    }
  }
  
  return result;
}