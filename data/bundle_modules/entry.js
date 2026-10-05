const math = require('./math');
const cycle = require('./cycle');
exports.started = true;
exports.result = [math.add(20, 22), math.total(1,2,3), require('./math') === math, cycle.entry() === exports];
