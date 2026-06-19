# PureScript + Vite + React Template

A modern, production-ready template for building highly typed, functional web applications using PureScript, React, Vite, and Tailwind CSS.

## Features

- **PureScript & Spago**: Strongly-typed functional programming configured with the modern `spago.yaml` system.
- **React & React-Basic**: Functional UI development utilizing PureScript React bindings for strict type safety in the DOM.
- **Vite**: Blazing fast frontend tooling with Hot Module Replacement (HMR) for instant feedback.
- **Tailwind CSS**: Utility-first CSS framework pre-configured for rapid, responsive styling.
- **DevContainer Included**: A reproducible, one-click development environment configured for VS Code and GitHub Codespaces.

## Prerequisites

If you are not using the included DevContainer or GitHub Codespaces, you will need the following installed on your local machine:

- [Node.js](https://nodejs.org/) (v18+ recommended)
- [PureScript](https://www.purescript.org/) (`npm i -g purescript`)
- [Spago](https://github.com/purescript/spago) (`npm i -g spago`)

## Getting Started

1. **Clone the repository**

   ```bash
   git clone https://github.com/JunghunLeePhD/purescript-template.git
   cd purescript-template
   ```

2. **Install Node dependencies**

   ```bash
   npm install
   ```

   *(Note: The `*package-lock.json*` is intentionally tracked in version control to guarantee identical, deterministic builds across all environments and deployment pipelines.)*

3. **Install PureScript dependencies**

   ```bash
   spago install
   ```

4. **Start the Development Server**

   ```bash
   npm run dev
   ```

This will start the Vite development server with Hot Module Replacement (HMR). Open your browser to `http://localhost:5173`.

## **Building for Production**

To compile your PureScript code and bundle it via Vite for production deployment (e.g., Vercel, Netlify):

```bash
npm run build
```

Your highly optimized static site will be generated in the `dist` folder.

## **Project Structure**

- `src/` - Your functional PureScript source code (`.purs` files).

- `index.html` - The main HTML entry point.

- `vite.config.js` - Vite configuration and PureScript plugin integration.

- `spago.yaml` / `spago.lock` - PureScript package management and dependency lockfiles.

- `package.json` / `package-lock.json` - Node.js ecosystem dependencies.

- `.devcontainer/` - Configuration for isolated, reproducible containerized development.

## **IDE Support**

For the optimal developer experience, we recommend using **VS Code** with the **PureScript IDE** extension.

_Note: The `_.psc-ide-port*` file is correctly ignored by `*.gitignore*` to prevent machine-specific IDE background ports from causing merge conflicts.*

## **🐳 Running in Docker (Zero Install)**

This repository includes a fully configured Docker development environment. You do not need to install Node, PureScript, or Spago on your local machine to use this template!

**Option 1: VS Code Dev Containers (Recommended)**

1. Install Docker on your machine.
2. Install the [Dev Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) extension in VS Code.
3. Clone this repository and open the folder in VS Code.
4. A prompt will appear in the bottom right: _"Folder contains a Dev Container configuration file."_ Click **Reopen in Container**.
5. VS Code will build the PureScript Docker image and connect to it. Open a terminal inside VS Code and run `npm run dev`!

**Option 2: GitHub Codespaces**
If you want to code entirely in the cloud without downloading Docker:

1. Click the green **Code** button at the top of this repository.
2. Switch to the **Codespaces** tab.
3. Click **Create codespace on main**.
4. A full VS Code environment will launch in your browser with all PureScript dependencies pre-installed.
