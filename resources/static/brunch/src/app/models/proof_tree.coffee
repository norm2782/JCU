ProofTreeNode = require('models/proof_tree_node').ProofTreeNode

class exports.ProofTree extends Backbone.Model
  # Available attributes:
  # treeRoot :: RuleTreeNode
  url: -> '/rules/inuse'

  mkTree: (raw, t) ->
    node = new ProofTreeNode()
    node.set({term: raw.term})
    cs = new Backbone.Collection()
    cs.add(t.mkTree ct, t) for ct in raw.childTerms
    node.set({childTerms: cs})
    node

  initialize: ->
    model = @
    options = {}
    options.success = (resp, status, xhr) ->
      # console.log resp
      r = model.mkTree resp, model
      # console.log r
      model.set({treeRoot: r})
    Backbone.sync.call(this, 'read', this, options)

  allValid: ->
    @get('treeRoot').isValid()
