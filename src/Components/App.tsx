import React from 'react';
import CodeEditor from './CodeEditor';
import ControlPanel from './ControlPanel';
import './App.css';
import {Container, Row, Col} from 'react-bootstrap';

function App() {
  return (
    <div className="App">
      <h1>
        Welcome to Haskpad, a collaborative code editor built with Haskell.
      </h1>
      <Container>
        <Row>
          <Col xs="1">
            <ControlPanel />
          </Col>
          <Col>
            <CodeEditor />
          </Col>
        </Row>
      </Container>
    </div>
  );
}

export default App;
