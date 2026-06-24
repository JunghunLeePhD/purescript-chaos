export const createWorker = () => {
  return new Worker(new URL("../../src/worker.js", import.meta.url), {
    type: "module",
  });
};

export const postStartMessage = (worker) => (msg) => () => {
  worker.postMessage(msg);
};

export const onRowMessage = (worker) => (callback) => () => {
  worker.onmessage = (e) => {
    callback(e.data)();
  };
};
