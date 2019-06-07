// @ts-check

const { Buffer } = require("buffer");
const fs = require("fs");
const upng = require("upng-js");
// @ts-ignore
const { Elm } = require("./main");
// @ts-ignore
const example = require("./example.json");

const app = Elm.Main.init({ flags: example });

app.ports.onError.subscribe(error => console.error("error", error));

app.ports.onTimings.subscribe(timings => console.log(timings));

app.ports.onImage.subscribe(imageBase64 => {
  const buffer = Buffer.from(imageBase64, "base64");
  const png = upng.encode(
    [new Uint8Array(buffer).buffer],
    example.size.width,
    example.size.height,
    0
  );
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
    }
  );
});
