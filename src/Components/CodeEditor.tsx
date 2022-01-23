import Editor from "@monaco-editor/react"

function CodeEditor() {
  return (
    <Editor
      height="90vh"
      defaultLanguage="haskell"
      defaultValue="// Welcome to Haskpad!"
    />
  );
};

export default CodeEditor;
