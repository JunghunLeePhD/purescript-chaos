export const onStartMessage = (callback) => () => {
  self.onmessage = (e) => {
    callback(e.data)();
  };
};

export const postRowToMain = (msg) => () => {
  self.postMessage(msg);
};
