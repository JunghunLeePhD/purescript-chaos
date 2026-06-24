export const getHardwareConcurrency = () => navigator.hardwareConcurrency || 4;

export const createWorker = () => {
  return new Worker(new URL("../../src/worker.js", import.meta.url), {
    type: "module",
  });
};

export const postToWorker = (worker) => (msg) => () => {
  worker.postMessage(msg);
};

export const onWorkerMessage = (worker) => (callback) => () => {
  worker.onmessage = (e) => callback(e.data)();
};
