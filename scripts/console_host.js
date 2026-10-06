// Identical deterministic console host for Node source, Hermes and exported JS.
var hermesHostLogs = [];
var console = {};
['log', 'warn', 'error', 'info', 'debug', 'trace'].forEach(function(method) {
  console[method] = function() {
    hermesHostLogs.push([method, Array.prototype.slice.call(arguments)]);
  };
});
