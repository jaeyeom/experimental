package main

import (
	"sort"
	"testing"
)

func TestPackagesSorted(t *testing.T) {
	if !sort.SliceIsSorted(packages, func(i, j int) bool {
		return packages[i].command < packages[j].command
	}) {
		t.Errorf("packages slice is not sorted by command name")
		for i := 1; i < len(packages); i++ {
			if packages[i].command < packages[i-1].command {
				t.Errorf("Package '%s' is out of order (comes after '%s')", packages[i].command, packages[i-1].command)
			}
		}
	}
}

func TestPlatformSpecificToolsSorted(t *testing.T) {
	if !sort.SliceIsSorted(platformSpecificTools, func(i, j int) bool {
		return platformSpecificTools[i].command < platformSpecificTools[j].command
	}) {
		t.Errorf("platformSpecificTools slice is not sorted by command name")
		for i := 1; i < len(platformSpecificTools); i++ {
			if platformSpecificTools[i].command < platformSpecificTools[i-1].command {
				t.Errorf("Tool '%s' is out of order (comes after '%s')", platformSpecificTools[i].command, platformSpecificTools[i-1].command)
			}
		}
	}
}
