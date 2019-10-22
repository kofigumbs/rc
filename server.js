const express = require("express");
const app = express();
app.use(express.static(".build/bitmoji-warp"));
app.listen(process.env.PORT || 3000);
