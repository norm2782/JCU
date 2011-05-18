ProofTreeNodeView = require('views/proof_tree_node_view').ProofTreeNodeView

class exports.ProofTreeView extends Backbone.View

  id: 'proof-tree-view'
  tagName: 'ul'
  className: 'tree'

  getRoot: ->
    # console.log @model
    # console.log @model.attributes
    # console.log @model.attributes.treeRoot
    @model.get('treeRoot')

  initialize: ->
    _.bindAll @, "render"
    @getRoot().get('childTerms').bind "change", @render

  render: ->
    view = new ProofTreeNodeView model: @getRoot()
    @$(@el).html view.render().el
