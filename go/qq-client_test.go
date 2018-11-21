package qq

import (
	"testing"
)

func TestBasic(t *testing.T) {
	client := NewClient("10.0.0.2:1234", nil)
	err := client.Push("test", &String{"abc"}, DefaultPriority)
	if err != nil {
		t.Fatal(err)
	}

	keys, err := client.List()
	if err != nil {
		t.Fatal(err)
	}

	if keys[0] != "test" {
		t.Log(keys)
		t.Fatal("Invalid list")
	}
}
