% Nodes for Reia's Binding Annotated Format
-record(bindings, {
  node,    % Original parse tree node, subject of this annotation 
  entries, % Binding dict when this node begins execution
  final,   % Binding dict when this node finishes execution
  output   % Dict of highest version number of any clause of this node
}).