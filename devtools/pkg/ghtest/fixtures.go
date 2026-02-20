package ghtest

import "strconv"

// Common test fixtures for GitHub API responses.

// SampleIssue returns a sample GitHub issue JSON response.
func SampleIssue(number int, title, body string) map[string]any {
	return map[string]any{
		"number": number,
		"title":  title,
		"body":   body,
		"state":  "open",
		"url":    "https://github.com/owner/repo/issues/" + strconv.Itoa(number),
	}
}

// SamplePR returns a sample GitHub pull request JSON response.
func SamplePR(number int, title string) map[string]any {
	return map[string]any{
		"number":      number,
		"title":       title,
		"state":       "open",
		"mergeable":   "MERGEABLE",
		"headRefName": "feature-branch",
		"url":         "https://github.com/owner/repo/pull/" + strconv.Itoa(number),
		"head": map[string]any{
			"sha": "abc123",
		},
		"base": map[string]any{
			"sha": "def456",
		},
	}
}

// SampleRepo returns a sample GitHub repository JSON response.
func SampleRepo(owner, name string) map[string]any {
	return map[string]any{
		"name":  name,
		"owner": map[string]any{"login": owner},
		"url":   "https://github.com/" + owner + "/" + name,
	}
}

// SampleUser returns a sample GitHub user JSON response.
func SampleUser(login string) map[string]any {
	return map[string]any{
		"login": login,
		"id":    12345,
		"type":  "User",
	}
}

// SampleAuthStatus returns a sample "gh auth status" JSON response.
func SampleAuthStatus(username, hostname string) map[string]any {
	return map[string]any{
		"github.com": map[string]any{
			"user":            username,
			"oauth_token":     "gho_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
			"git_protocol":    "https",
			"hostname":        hostname,
			"active_hostname": true,
		},
	}
}

// SamplePRFiles returns a sample PR files API response.
func SamplePRFiles() []map[string]any {
	return []map[string]any{
		{
			"filename":  "src/main.go",
			"status":    "modified",
			"additions": 10,
			"deletions": 5,
			"changes":   15,
			"patch":     "@@ -1,5 +1,10 @@\n+package main\n+\n+func main() {\n+}",
		},
		{
			"filename":  "README.md",
			"status":    "added",
			"additions": 20,
			"deletions": 0,
			"changes":   20,
		},
	}
}
