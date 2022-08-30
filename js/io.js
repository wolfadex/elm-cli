const fs = require("fs");
const crypto = require("crypto");

let streams = {};
let lastKey = 1;

function Ok(v) {
  return {
    result: "Ok",
    data: v,
  };
}

function Err(e) {
  return {
    result: "Err",
    data: e,
  };
}

function encodeError(err) {
  return Err({
    code: err.code || "NONE",
    msg: err.message,
  });
}

function Just(v) {
  return {
    result: "Just",
    data: v,
  };
}

function Nothing() {
  return {
    result: "Nothing",
    data: null,
  };
}

function maybeFromNullable(v) {
  if (v == null) {
    return Nothing();
  } else {
    return Just(v);
  }
}

module.exports = {
  readFile: function (cb, name) {
    try {
      const content = fs.readFileSync(name).toString();
      cb(Ok(content));
    } catch (err) {
      cb(encodeError(err));
    }
  },
  writeFile: function (cb, name, content, options) {
    try {
      fs.writeFileSync(name, content, options);
      cb(Ok(null));
    } catch (err) {
      cb(encodeError(err));
    }
  },
  fopen: function (cb, filename, flags) {
    try {
      cb(fs.openSync(filename, flags));
    } catch (e) {
      cb(e.toString());
    }
  },
  fstat: function (cb, filename) {
    try {
      cb(fs.statSync(filename));
    } catch (e) {
      cb(e.toString());
    }
  },
  readdir: function (cb, dirname) {
    try {
      var r = fs
        .readdirSync(dirname, { withFileTypes: true })
        .map((dirent) => ({
          name: dirent.name,
          isDir: dirent.isDirectory(),
          isFile: dirent.isFile(),
          isSocket: dirent.isSocket(),
          isFifo: dirent.isFIFO(),
          isSymlink: dirent.isSymbolicLink(),
          isBlockDevice: dirent.isBlockDevice(),
          isCharacterDevice: dirent.isCharacterDevice(),
        }));
      cb(r);
    } catch (e) {
      cb(e.toString());
    }
  },
  randomSeed: function (cb) {
    cb(crypto.randomBytes(4).readInt32LE());
  },
  print: function (cb, str) {
    fs.writeFileSync(1, str);
    cb();
  },
  panic: function (cb, msg) {
    console.error(msg);
    process.exit(255);
  },
  exit: function (cb, status) {
    process.exit(status);
  },

  // DIRECTORY

  stat: function (cb, path) {
    try {
      let stat = fs.statSync(path);
      stat.absolutePath = fs.realpathSync(path);
      setFileType(stat);
      cb(Ok(stat));
    } catch (err) {
      cb(encodeError(err));
    }
  },
  listDir: function (cb, path) {
    try {
      let dirPath = fs.realpathSync(path);
      let entries = fs.readdirSync(path, {
        withFileTypes: true,
        encoding: "utf8",
      });
      entries.forEach((ent) => {
        setFileType(ent);
        ent.absolutePath = dirPath + "/" + ent.name;
      });
      cb(Ok(entries));
    } catch (err) {
      cb(encodeError(err));
    }
  },
  // Environment functions
  getEnv: function (cb, key) {
    cb(maybeFromNullable(process.env[key]));
  },
  getArgs: function (cb) {
    cb(process.argv.slice(5));
  },

  // Streams

  openReadStream: function (cb, filename, bufferSize) {
    var key = "read-" + ++lastKey;

    try {
      var file = fs.openSync(filename);
    } catch (err) {
      cb(encodeError(err));
    }

    streams[key] = (it) => readGenerator(file, bufferSize);

    cb(Ok({ id: key }));
  },
  openWriteStream: function (cb, filename, options) {
    var key = "write-" + ++lastKey;

    try {
      var file = fs.openSync(filename, options.flag, options.mode);
    } catch (err) {
      cb(encodeError(err));
    }

    streams[key] = (it) => writeGenerator(file, it);

    cb(Ok({ id: key }));
  },
  readStream: function (cb, pipes) {
    const key = piplineKey(pipes);

    let iterator = streams[key];

    if (!iterator) {
      iterator = createPipeline(pipes);
      streams[key] = iterator;
    }

    let val = null;

    try {
      val = iterator.next().value;
    } catch (err) {
      cb(encodeError(err));
    }

    if (val == undefined) {
      cb(Ok(null));
    } else if (val instanceof Buffer) {
      cb(Ok([...val.values()]));
    }

    cb(Ok(val));
  },
  writeStream: function (cb, pipes, data) {
    if (Array.isArray(data)) {
      data = Buffer.from(data);
    }
    let iterator = createPipeline(pipes, valueToIterator(data));
    let bytesWritten = iterator.next().value;

    cb(Ok(bytesWritten));
  },

  // Net
  "http:createServer": function (cb, options) {
    var key = "http-server" + ++lastKey;
    try {
      var http = require("node:http");
      var server = http.createServer(function (request, response) {
        cb(Ok({ request, response }));
      });
      server.listen(options.port, options.host, function () {});
    } catch (err) {
      return encodeError(err);
    }
  },
  // openReadStream: function (filename, bufferSize) {
  //   var key = "read-" + ++lastKey;

  //   try {
  //     var file = fs.openSync(filename);
  //   } catch (err) {
  //     return encodeError(err);
  //   }

  //   streams[key] = (it) => readGenerator(file, bufferSize);

  //   return Ok({ id: key });
  // },
};

function setFileType(stat) {
  if (stat.isDirectory()) {
    stat.fileType = "Dir";
  } else if (stat.isFile()) {
    stat.fileType = "File";
  } else if (stat.isSocket()) {
    stat.fileType = "Socket";
  } else if (stat.isFIFO()) {
    stat.fileType = "FIFO";
  } else if (stat.isSymbolicLink()) {
    stat.fileType = "SymbolicLink";
  } else if (stat.isBlockDevice()) {
    stat.fileType = "BlockDevice";
  } else if (stat.isCharacterDevice()) {
    stat.fileType = "CharactedDevice";
  }
}

function piplineKey(pipes) {
  return "pipe:" + pipes.map((p) => p.id).join(":");
}

function createPipeline(pipes, iterator) {
  let pipe = null;

  while ((pipe = pipes.shift())) {
    switch (pipe.id) {
      case "utf8Decode":
        iterator = utf8Decode(iterator);
        break;

      case "utf8Encode":
        iterator = utf8Encode(iterator);
        break;

      case "line":
        iterator = splitLine(iterator);
        break;

      default:
        iterator = streams[pipe.id](iterator);
    }
  }

  return iterator;
}

function* splitLine(iterator) {
  let val = iterator.next().value;
  let buf = [];

  while (val) {
    let lines = val.split("\n");

    if (lines.length == 1) {
      buf.push(lines[0]);
    } else {
      let last = lines.pop();
      let prev = buf.join("");
      let first = lines.shift();
      buf = [last];

      yield prev + first;
      for (let line of lines) {
        yield line;
      }
    }

    val = iterator.next().value;
  }

  if (buf.length > 0) {
    yield buf.join("");
  }
}

function* utf8Encode(iterator) {
  let data = iterator.next().value;

  while (data) {
    if (data instanceof Buffer) {
      yield data;
    } else {
      yield Buffer.from(data);
    }

    data = iterator.next().value;
  }
}

function* utf8Decode(it) {
  let buffer = it.next(0).value;
  let partialMbBuffer = null;

  while (buffer) {
    const mbOffsetFromEnd = utf8_mbOffsetFromEnd(buffer);

    if (!mbOffsetFromEnd) {
      // No broken mb characters at the end of the buffer.
      yield buffer.toString("utf8");
    } else {
      // We have a partial multibyte char at the end.of the buffer.
      // yield everythin but the partial multibyte char.
      yield buffer.toString("utf8", 0, buffer.length - mbOffsetFromEnd);

      // Copy the partial multibyte char to the beginning of the buffer.
      buffer.copy(buffer, 0, buffer.length - mbOffsetFromEnd, buffer.length);
    }

    // Load more data into the buffer with offset.
    buffer = it.next(mbOffsetFromEnd).value;
  }
}

function utf8_mbOffsetFromEnd(buf) {
  const lastIdx = buf.length - 1;
  let idx = 1;
  let mbWidth = utf8_getMbWidth(buf[lastIdx]);

  if (!mbWidth) {
    // last byte is not multibyte.
    return 0;
  }

  while (true) {
    if (mbWidth == 1) {
      // we got a tail byte of a multibyte char
      // continue to search for the start byte.
      mbWidth = utf8_getMbWidth(buf[lastIdx - idx]);
      idx++;
    } else {
      // we got the start byte of a multibyte char.
      if (idx == mbWidth) {
        return 0;
      }
      return idx;
    }
  }
}

function utf8_getMbWidth(b) {
  // 1xxx xxxx
  if (b & 0x80) {
    if ((b & 0xf0) === 0xf0) {
      // 1111 xxxx
      // start of 4 byte char
      return 4;
    } else if ((b & 0xe0) === 0xe0) {
      // 111x xxxx
      // start of 3 byte char
      return 3;
    } else if ((b & 0xc0) === 0xc0) {
      // 11xx xxxx
      // start of 2 byte char
      return 2;
    }
    // Tail of mb char.
    return 1;
  }

  // Not a multi byte char.
  return 0;
}

function* readGenerator(fd, bufferSize) {
  const buffer = Buffer.alloc(bufferSize);
  let offset = 0;
  let bytesRead = fs.readSync(fd, buffer, offset, buffer.length - offset, null);

  while (bytesRead) {
    if (bytesRead < buffer.length - offset) {
      offset = yield buffer.slice(0, bytesRead);
    } else {
      offset = yield buffer;
    }
    bytesRead = fs.readSync(fd, buffer, offset, buffer.length - offset, null);
  }
}

function* writeGenerator(fd, iterator) {
  let data = iterator.next().value;

  while (data) {
    yield fs.writeSync(fd, data);
    data = iterator.next().value;
  }
}

function* valueToIterator(data) {
  yield data;
}

function* serverHandlerGenerator(request, response) {
  //
}
