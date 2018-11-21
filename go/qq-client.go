package qq

import (
	"errors"

	"github.com/go-redis/redis"
)

type Options struct {
	Password string
	DB       int
}

// Data allows for various datatype to be sent to qq
type Data interface {
	ToString() string
	FromString(string) error
}

var DefaultPriority = 100

// JSON is used to pass json data to the qq server
type JSON struct {
	data map[string]interface{}
}

type String struct {
	string
}

func (s String) ToString() string {
	return s.string
}

func (s *String) FromString(input string) error {
	s.string = input
	return nil
}

// Client stores a connection to a qq server
type Client struct {
	*redis.Client
}

func NewClient(url string, options *Options) Client {
	if options == nil {
		options = new(Options)
	}

	roptions := redis.Options{
		Addr:     url,
		Password: options.Password,
		DB:       options.DB,
	}

	client := redis.NewClient(&roptions)
	return Client{client}
}

func (client Client) Push(key string, data Data, priority int) error {
	_, err := client.Do("push", key, data.ToString(), priority).Result()
	if err != nil {
		return err
	}
	return nil
}

func (client Client) Pop(key string, data Data) (int64, error) {
	res, err := client.Do("pop", key).Result()
	if err != nil {
		return -1, err
	}

	var ok bool
	var arr []interface{}

	if arr, ok = res.([]interface{}); !ok {
		return 0, errors.New("Invalid response")
	}

	var s string
	if s, ok = arr[0].(string); !ok {
		return 0, errors.New("Invalid data")
	}

	if err := data.FromString(s); err != nil {
		return 0, err
	}

	var i int64
	if i, ok = arr[1].(int64); !ok {
		return 0, errors.New("Invalid priority")
	}

	return int64(i), nil
}

func (client Client) Del(key string) error {
	_, err := client.Do("del", key).Result()
	if err != nil {
		return err
	}

	return nil
}

func (client Client) Length(key string) (int64, error) {
	return client.Do("length", key).Int64()
}

func (client Client) List() ([]string, error) {
	res, err := client.Do("list").Result()
	if err != nil {
		return nil, err
	}

	var arr []interface{}
	var ok bool

	if arr, ok = res.([]interface{}); !ok {
		return nil, errors.New("Invalid response")
	}

	dest := []string{}

	for _, v := range arr {
		if s, ok := v.(string); ok {
			dest = append(dest, s)
		}
	}

	return dest, nil
}
