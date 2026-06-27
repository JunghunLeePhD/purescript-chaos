export const getRequest = (callback) => () => {
  self.onmessage = (e) => callback(e.data)();
};

export const sendResponse = (msg) => () => {
  self.postMessage(msg);
};
