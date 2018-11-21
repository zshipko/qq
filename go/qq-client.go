package qq

import (
	"bytes"
	"crypto/tls"
	"encoding/json"
	"errors"
	"strings"
	"time"

	"github.com/go-redis/redis"
)

// Options are used for passing extra parameters to NewClient
type Options struct {
	Password     string
	DB           int
	TLSConfig    *tls.Config
	DialTimeout  time.Duration
	ReadTimeout  time.Duration
	WriteTimeout time.Duration
	PoolSize     int
}

// Data allows for various datatype to be sent to qq
type Data interface {
	ToString() string
	FromString(string) error
}

// DefaultPriority is the default priority for new messaged
var DefaultPriority = 100

// JSON is used to pass json data to the qq server
type JSON struct {
	Data map[string]interface{}
}

// ToString implementation for JSON
func (j JSON) ToString() string {
	var buffer bytes.Buffer
	enc := json.NewEncoder(&buffer)
	enc.Encode(j.Data) // ignore error
	return buffer.String()
}

// FromString implementation for JSON
func (j *JSON) FromString(input string) error {
	r := strings.NewReader(input)
	dec := json.NewDecoder(r)
	return dec.Decode(&j.Data)
}

// String datatype
type String struct {
	string
}

// ToString implementation for String
func (s String) ToString() string {
	return s.string
}

// FromString implementation for String
func (s *String) FromString(input string) error {
	s.string = input
	return nil
}

// Client stores a connection to a qq server
type Client struct {
	*redis.Client
}

// NewClient creates a new client object
func NewClient(url string, options *Options) Client {
	if options == nil {
		options = new(Options)
	}

	roptions := redis.Options{
		Addr:         url,
		Password:     options.Password,
		DB:           options.DB,
		TLSConfig:    options.TLSConfig,
		DialTimeout:  options.DialTimeout,
		ReadTimeout:  options.ReadTimeout,
		WriteTimeout: options.WriteTimeout,
		PoolSize:     options.PoolSize,
	}

	client := redis.NewClient(&roptions)
	return Client{client}
}

// Push a message to the server
func (client Client) Push(key string, data Data, priority int) error {
	_, err := client.Do("push", key, data.ToString(), priority).Result()
	if err != nil {
		return err
	}
	return nil
}

// Pop a message from the server
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

// Del removes a key from the server
func (client Client) Del(key string) error {
	_, err := client.Do("del", key).Result()
	if err != nil {
		return err
	}

	return nil
}

// Length returns the number of items in the queue associated with the given key
func (client Client) Length(key string) (int64, error) {
	return client.Do("length", key).Int64()
}

// List provides a list of available keys
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
