class exports.ProofTreeNode extends Backbone.Model
  # Available attributes:
  # term :: Term
  # childTerms :: BackBone.Collection

  initialize: =>
    @resetChildren()

  hasTerm: =>
    @get('term')?

  getChildTerms: =>
    @get('childTerms')

  resetChildren: =>
    @set {childTerms: new Backbone.Collection()}

  setChildNo: (childNo) =>
    @resetChildren()
    @addRule() for i in [1..childNo] if childNo > 0

  addRule: =>
    @getChildTerms().add(new ProofTreeNode())
