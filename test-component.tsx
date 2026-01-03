// Simple React component for testing
import React from 'react';

interface Props {
  name: string;
}

const HelloWorld: React.FC<Props> = ({ name }) => {
  return (
    <div className="hello-world">
      <h1>Hello, {name}!</h1>
      <p>This is a test React component with TypeScript.</p>
    </div>
  );
};

export default HelloWorld;