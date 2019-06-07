// @ts-check

const { Buffer } = require("buffer");
const fs = require("fs");
const upng = require("upng-js");
// @ts-ignore
const { Elm } = require("./main");

const filename = "example_mask.png";

fs.readFile(filename, (error, fileBuffer) => {
  if (error) {
    throw error;
  }

  const maskImage = upng.decode(fileBuffer);
  const { width, height } = maskImage;

  /** @type {ArrayBuffer[]} */
  const maskFrames = upng.toRGBA8(maskImage);
  const bytesString = Buffer.from(maskFrames[0]).toString("base64");

  const app = Elm.Main.init({ flags: { width, height, bytesString } });

  app.ports.onError.subscribe(error => console.error("error", error));

  app.ports.onTimings.subscribe(timings => console.log(timings));

  app.ports.onTrack.subscribe(track =>
    fs.writeFile("example.json", JSON.stringify(track, null, "  "), error => {
      if (error) {
        console.error("write error", error);
      } else {
        console.log("file written");
      }
    })
  );
});
