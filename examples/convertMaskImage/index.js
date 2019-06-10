// @ts-check

const { Buffer } = require("buffer");
const fs = require("fs");
const upng = require("upng-js");
// @ts-ignore
const { Elm } = require("./main");

console.time("convertMaskImage");

fs.readFile("example_mask.png", (error, fileBuffer) => {
  if (error) {
    throw error;
  }

  const maskImage = upng.decode(fileBuffer);
  const { width, height } = maskImage;

  /** @type {ArrayBuffer[]} */
  const maskFrames = upng.toRGBA8(maskImage);
  const maskFrameArray = new Uint8Array(maskFrames[0]);
  const bytesArray = Array.from(maskFrameArray);

  const app = Elm.Main.init({ flags: { width, height, bytesArray } });

  app.ports.onError.subscribe(error => console.error("error", error));

  app.ports.onTimings.subscribe(timings => console.log(timings));

  app.ports.onTrack.subscribe(track =>
    fs.writeFile("example.json", JSON.stringify(track, null, "  "), error => {
      if (error) {
        console.error("write error", error);
      } else {
        console.log("file written");
      }
      console.timeEnd("convertMaskImage");
    })
  );
});
