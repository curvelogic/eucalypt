package tree_sitter_eucalypt_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-eucalypt"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_eucalypt.Language())
	if language == nil {
		t.Errorf("Error loading Eucalypt grammar")
	}
}
