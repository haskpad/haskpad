import CodeMirror from '@uiw/react-codemirror';


function CodeEditor() {

  return (
    <CodeMirror
      value="// Welcome to Haskpad"
      height="90vh"
      onChange={(value, viewUpdate) => {
        console.log('value:', value);
      }}
    />
  );
};

export default CodeEditor;
