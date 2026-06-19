import { initializeTodo } from "../output/Main/index.js";

class TodoApp extends HTMLElement {
  connectedCallback() {
    initializeTodo(this)();
  }
}

customElements.define("todo-app", TodoApp);
