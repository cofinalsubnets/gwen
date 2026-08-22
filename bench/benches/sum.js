// build the list 1..100000 then sum it -- allocation + traversal.
const { bench } = require("../lib/bench");
const data = Array.from({ length: 100000 }, (_, i) => i + 1);
bench("sum", () => data.reduce((a, b) => a + b, 0));
