from flask import Flask, templating, send_from_directory, request, jsonify, redirect, url_for
import werkzeug.datastructures
from htmlmin.minify import html_minify
from sshtunnel import SSHTunnelForwarder
import os
import psycopg2
import psycopg2.extensions
import hashlib
import socket
import secrets

app = Flask(__name__)
app.config['MAX_CONTENT_LENGTH'] = 1 * 1024 * 1024

app.jinja_env.globals.update(debug=app.debug)


def get_free_port() -> int:
    sock = socket.socket()
    sock.bind(('', 0))
    ip, port = sock.getsockname()
    sock.close()
    return port


def connect_to_database():
    global sshforward

    cwf_user = os.environ['CWF_USER']
    cwf_pass = os.environ['CWF_PASS']

    cwf_use_ssh = os.getenv('CWF_USE_SSH', "0")

    if cwf_use_ssh == "1":
        cwf_host = os.environ['CWF_HOST']
        cwf_port = int(os.environ['CWF_PORT'])
        cwf_pkey = os.environ['CWF_PKEY']

        available_port = get_free_port()

        sshforward = SSHTunnelForwarder((cwf_host, 22),
                                        ssh_pkey=cwf_pkey,
                                        remote_bind_address=('localhost', cwf_port),
                                        local_bind_address=('localhost', available_port))

        sshforward.start()
    else:
        available_port = 5432

    conn = psycopg2.connect(
        "postgresql://{user:s}:{password:s}@localhost:{port:d}/rockstar".format(user=cwf_user,
                                                                                  password=cwf_pass,
                                                                                  port=available_port))
    # type: psycopg2.extensions.connection

    return conn


connection = connect_to_database()  # type: psycopg2.extensions.connection


@app.after_request
def minify_request(response):
    if response.content_type == u'text/html; charset=utf-8':
        response.set_data(
            html_minify(response.get_data(as_text=True))
        )

        return response
    return response


@app.route('/static/js/<path:jspath>')
def static_js(jspath):
    return send_from_directory("../bundle/static/js", jspath)


@app.route('/api/test/rehash_all')
def rehash_all():
    with connection:
        with connection.cursor() as c: # type: psycopg2._ext.cursor
            with connection.cursor() as wc:  # type: psycopg2._ext.cursor
                c.execute('SELECT shortlink, fulltext FROM shortlinks')

                for row in c:
                    hasher = hashlib.sha512()
                    hasher.update(row[1].encode('utf8', errors='replace'))
                    hash = hasher.digest()

                    wc.execute('UPDATE shortlinks SET fulltext_hash = %s WHERE shortlink = %s', (hash, row[0]))

            return "", 204


@app.route('/api/gen_shortlink', methods=['POST'])
def gen_shortlink():
    if 'file' not in request.files:
        return jsonify({"error": "no file uploaded"}), 400
    file = request.files['file'] # type: werkzeug.datastructures.FileStorage

    with connection:
        with connection.cursor() as c: # type: psycopg2._ext.cursor
            raw_file_text = file.stream.read()
            file_text = raw_file_text.decode('utf8', errors='replace').replace('\0', '')

            hasher = hashlib.sha512()
            hasher.update(raw_file_text)
            hash = hasher.digest()

            c.execute('SELECT shortlink FROM shortlinks WHERE fulltext_hash = %s', (hash,))

            if c.rowcount != 0:
                return jsonify({"link": c.fetchone()[0]})

            while True:
                id = secrets.token_urlsafe(8)

                try:
                    c.execute('INSERT INTO shortlinks (shortlink, fulltext, fulltext_hash) VALUES (%s, %s, %s)', (id, file_text, hash))
                except psycopg2.IntegrityError:
                    continue

                return jsonify({"link": id})


@app.route('/<string:link>')
def short_link(link):
    with connection:
        with connection.cursor() as c: # type: psycopg2._ext.cursor
            c.execute('SELECT fulltext FROM shortlinks WHERE shortlink = %s', (link,))

            if c.rowcount != 0:
                return templating.render_template("index.html", input=c.fetchone()[0])
            else:
                return redirect(url_for('homepage'))


@app.route('/')
def homepage():
    return templating.render_template("index.html", input="")


if __name__ == '__main__':
    app.run()
