"use strict";

const {
  JSONRPCServerAndClient,
  JSONRPCServer,
  JSONRPCClient,
} = require("json-rpc-2.0");

class JSONRPC {
  constructor(webSocket) {
    this.messageQueue = [];
    this.webSocket = webSocket;
    this.serverAndClient = this.connect(webSocket);
  }

  on(method, handler) {
    this.serverAndClient.addMethod(method, handler);
  }

  async requestInternal(method, arg) {
    const result = await this.serverAndClient.request(method, arg);
    console.log(result);
  }

  requestMessageQueue() {
    this.messageQueue.forEach((value) => {
      const [method, arg] = value;
      this.requestInternal(method, arg);
    });
    this.messageQueue = [];
  }

  request(method, arg) {
    if (this.webSocket.readyState === WebSocket.OPEN) {
      this.requestInternal(method, arg);
    } else {
      this.messageQueue.push([method, arg]);
    }
  }

  notify(method, arg) {
    return this.serverAndClient.notify(method, arg);
  }

  connect(webSocket) {
    const serverAndClient = new JSONRPCServerAndClient(
      new JSONRPCServer(),
      new JSONRPCClient((request) => {
        try {
          webSocket.send(JSON.stringify(request));
          return Promise.resolve();
        } catch (error) {
          return Promise.reject(error);
        }
      }),
    );

    webSocket.onmessage = (event) => {
      console.log(event);
      serverAndClient.receiveAndSend(JSON.parse(event.data.toString()));
    };

    webSocket.onclose = (event) => {
      serverAndClient.rejectAllPendingRequests(
        `Connection is closed (${event.reason}).`,
      );
    };

    webSocket.onopen = () => {
      console.log("WebSocket connection established");
      this.requestMessageQueue();
    };

    webSocket.onerror = (error) => {
      console.error("WebSocket error:", error);
    };

    return serverAndClient;
  }
}

exports.JSONRPC = JSONRPC;
