const browserSync = require('browser-sync').create();
const historyApiFallback = require('connect-history-api-fallback');

browserSync.init({
  server: "example",
  files: ["example/index.html", "example/output/bundle.js"],
  middleware: [historyApiFallback()]
});
