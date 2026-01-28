(function_definition
  body: (_) @function.inner) @function.outer

(struct_definition
  body: (_) @class.inner) @class.outer

(if_expression
  consequence: (_) @block.inner) @block.outer

(while_expression
  body: (_) @block.inner) @block.outer

(for_expression
  body: (_) @block.inner) @block.outer

(loop_expression
  body: (_) @block.inner) @block.outer

(block
  body: (_) @block.inner) @block.outer

(closure_literal
  body: (_) @block.inner) @block.outer
