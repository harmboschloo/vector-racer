// @ts-check

const { Buffer } = require("buffer");
const fs = require("fs");
const upng = require("upng-js");
// @ts-ignore
const { Elm } = require("./main");
// @ts-ignore
const example = require("./example.json");

console.time("generateTrackImage");

const app = Elm.Main.init({ flags: example });

app.ports.onError.subscribe(error => console.error("error", error));

app.ports.onTimings.subscribe(timings => console.log(timings));

app.ports.onImage.subscribe(image => {
  const buffer = new Uint8Array(image).buffer;
  // const buffer = new Uint8Array(Buffer.from(image, "base64")).buffer;
  const png = upng.encode([buffer], example.size.width, example.size.height, 0);
  fs.writeFile(
    "example.png",
    Buffer.from(new Uint8Array(png)),
    "binary",
    error => {
      if (error) {
        console.error("write error", error);
      } else {
        console.log("file written");
      }
      console.timeEnd("generateTrackImage");
    }
  );
});
