ProofTreeNodeView = require('views/proof_tree_node_view').ProofTreeNodeView

class exports.ProofTreeView extends Backbone.View

  id: 'proof-tree-view'
  tagName: 'ul'
  className: 'tree'

  initialize: ->
    _.bindAll @, "render"
    @model.bind "change", @render

  render: ->
    view = new ProofTreeNodeView model: @model.get('root')
    @$(@el).html view.render().el
