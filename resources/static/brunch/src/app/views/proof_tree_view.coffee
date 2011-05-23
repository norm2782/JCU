ProofTreeNodeView = require('views/proof_tree_node_view').ProofTreeNodeView

class exports.ProofTreeView extends Backbone.View

  id: 'proof-tree-view'
  tagName: 'ul'
  className: 'tree'

  getRoot: =>
    @model.get('treeRoot')

  render: =>
    view = new ProofTreeNodeView({model: @getRoot(), id: "view_" + @getRoot().cid})
    @$(@el).html view.render().el
