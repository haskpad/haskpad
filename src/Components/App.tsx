import React from 'react';
import CodeEditor from './CodeEditor';
import ControlPanel from './ControlPanel';
import './App.css';


function App() {
  return (
    <div className="App">
      <h1>
        Welcome to Haskpad, a collaborative code editor built with Haskell.
      </h1>
      <CodeEditor />
    </div>
  );
}

export default App;
