

# Module prometheus_http #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

HTTP instrumentation helpers.

<a name="types"></a>

## Data Types ##




### <a name="type-status_class">status_class()</a> ###


<pre><code>
status_class() = <a href="prometheus_model_helpers.md#type-label_value">prometheus_model_helpers:label_value()</a>
</code></pre>




### <a name="type-status_code">status_code()</a> ###


<pre><code>
status_code() = pos_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#microseconds_duration_buckets-0">microseconds_duration_buckets/0</a></td><td>
Returns default microseconds buckets for measuring http requests duration.</td></tr><tr><td valign="top"><a href="#negotiate-2">negotiate/2</a></td><td>
Negotiate the most appropriate content_type given the accept header
and a list of alternatives.</td></tr><tr><td valign="top"><a href="#status_class-1">status_class/1</a></td><td>
Returns status class for the http status code <code>SCode</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="microseconds_duration_buckets-0"></a>

### microseconds_duration_buckets/0 ###

<pre><code>
microseconds_duration_buckets() -&gt; <a href="prometheus_buckets.md#type-buckets">prometheus_buckets:buckets()</a>
</code></pre>
<br />

Returns default microseconds buckets for measuring http requests duration.

```erlang

  1> prometheus_http:microseconds_duration_buckets().
  [10, 25, 50, 100, 250, 500,
   1000, 2500, 5000, 10000, 25000, 50000, 100000, 250000, 500000,
   1000000, 2500000, 5000000, 10000000]
```

<a name="negotiate-2"></a>

### negotiate/2 ###

<pre><code>
negotiate(Header, Alternatives) -&gt; Match
</code></pre>

<ul class="definitions"><li><code>Header = BinaryOrString</code></li><li><code>Alternatives = [Alternative]</code></li><li><code>Alternative = BinaryOrString | {BinaryOrString | Tag}</code></li><li><code>BinaryOrString = binary() | string()</code></li><li><code>Tag = any()</code></li><li><code>Match = Tag | nomatch</code></li></ul>

Negotiate the most appropriate content_type given the accept header
and a list of alternatives.

Ported from [goautoneg](https://bitbucket.org/ww/goautoneg).

```
  Copyright (c) 2011, Open Knowledge Foundation Ltd.
  All rights reserved.
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:
      Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
      Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the
      distribution.
      Neither the name of the Open Knowledge Foundation Ltd. nor the
      names of its contributors may be used to endorse or promote
      products derived from this software without specific prior written
      permission.
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```

<a name="status_class-1"></a>

### status_class/1 ###

<pre><code>
status_class(SCode) -&gt; StatusClass
</code></pre>

<ul class="definitions"><li><code>SCode = <a href="#type-status_code">status_code()</a></code></li><li><code>StatusClass = <a href="#type-status_class">status_class()</a></code></li></ul>

Returns status class for the http status code `SCode`.

```erlang

  2> prometheus_http:status_class(202).
  "success"
```

Raises `{invalid_value_error, SCode, Message}` error if `SCode`
isn't a positive integer.

