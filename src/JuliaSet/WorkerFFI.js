export const onTaskMessage = (callback) => () => {
  self.onmessage = (e) => callback(e.data)();
};

export const postResultToMain = (msg) => () => {
  self.postMessage(msg);
};
