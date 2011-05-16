ProofTreeNode = require('models/proof_tree_node').ProofTreeNode

class exports.ProofTree extends Backbone.Model
  # Available attributes:
  # root :: RuleTreeNode
  url: -> '/rules/inuse'

  mkTree: (raw, t) ->
    node = new ProofTreeNode()
    node.set({term: raw.term})
    cs = new Backbone.Collection(cs)
    cs.add(t.mkTree ct, t) for ct in raw.childTerms
    node.set({childTerms: cs})
    node

  initialize: ->
    model = @
    options = {}
    options.success = (resp, status, xhr) ->
      r = model.mkTree resp, model
      console.log r
      model.set({root: r})
    Backbone.sync.call(this, 'read', this, options)

  allValid: ->
    @get('root').isValid()
