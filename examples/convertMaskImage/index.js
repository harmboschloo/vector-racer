// @ts-check

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
  const bytesFile = new File(maskFrames[0], filename);

  const app = Elm.Main.init({ flags: { width, height, bytesFile } });

  app.ports.onError.subscribe(error => console.error("error", error));

  app.ports.onTrack.subscribe(track =>
    fs.writeFile(
      "example.json",
      JSON.stringify(track, null, "  "),
      error => error && console.error("write error", error)
    )
  );
});

// Hack to work with elm/File in node //

class File {
  constructor(buffer, name) {
    this.buffer = buffer;
    this.size = buffer.byteLength;
    this.name = name;
    this.type = "application/octet-stream";
  }
}

class FileReader {
  addEventListener(name, callback) {
    this.callback = callback;
  }

  readAsArrayBuffer(file) {
    this.result = file.buffer;
    this.callback();
  }
}

// @ts-ignore
global.File = File;
// @ts-ignore
global.FileReader = FileReader;
