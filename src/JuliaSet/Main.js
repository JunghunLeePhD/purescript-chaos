export const getHardwareConcurrency = () => navigator.hardwareConcurrency || 4;

export const createWorker = () => {
  return new Worker(new URL("/src/JuliaSet/worker.js", import.meta.url), {
    type: "module",
  });
};

export const sendRequest = (worker) => (msg) => () => {
  worker.postMessage(msg);
};

export const getResponse = (worker) => (callback) => () => {
  worker.onmessage = (e) => callback(e.data)();
};
