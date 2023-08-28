async function scenario1() {
    const req = async () => {
        const resp = await fetch("http://localhost:8080/1")
        return resp.text()
    }

    return Promise.race([req(), req()])
}

async function scenario2() {
    const req = async () => {
        const resp = await fetch("http://localhost:8080/2")
        return resp.text()
    }

    return Promise.any([req(), req()])
}

async function scenario3() {
    const req = async () => {
        const resp = await fetch("http://localhost:8080/3")
        return resp.text()
    }
    const reqs = Array.from(new Array(10000), req)
    return Promise.race(reqs)
}

async function scenario4() {
    const req = async () => {
        const resp = await fetch("http://localhost:8080/4")
        return resp.text()
    }

    const reqWithTimeout = async () => {
        const controller = new AbortController();
        const signal = controller.signal;

        const requestWithSignal = fetch("http://localhost:8080/4" , { signal })
            .then((resp) => resp.text())

        const timeoutPromise = new Promise((_, reject) => {
            setTimeout(() => {
                controller.abort()
                reject(new Error("Request timed out"))
            }, 1000)
        })
        return Promise.race([timeoutPromise, requestWithSignal])
    }

    return Promise.any([req(), reqWithTimeout()])
}

async function scenario5() {
    const req = async () => {
        const resp = await fetch("http://localhost:8080/5")
        if (resp.status >= 300) {
            return Promise.reject(new Error("Not a successful response"))
        }
        else {
            return resp.text()
        }
    }

    return Promise.any([req(), req()])
}

async function scenario6() {
    const req = async () => {
        const resp = await fetch("http://localhost:8080/6")
        if (resp.status >= 300) {
            return Promise.reject(new Error("Not a successful response"))
        }
        else {
            return resp.text()
        }
    }

    return Promise.any([req(), req(), req()])
}

async function scenario7() {
    const req = async () => {
        const resp = await fetch("http://localhost:8080/7")
        return resp.text()
    }
    const hedge = async () => {
        await new Promise(() => setTimeout(req, 3000));
    }

    return Promise.race([req(), hedge()])
}

async function scenario8() {
    const req = async (params) => {
        const resp = await fetch(`http://localhost:8080/8?${params}`)
        if (resp.status >= 300) {
            return Promise.reject(new Error("Not a successful response"))
        }
        else {
            return resp.text()
        }
    }

    const open = () => req("open")
    const use = (id) => req(`use=${id}`)
    const close = (id) => req(`close=${id}`)

    const reqRes = async () => {
        const id = await open()
        try {
            return await use(id)
        }
        finally {
            await close(id)
        }
    }

    return Promise.any([reqRes(), reqRes()])
}

async function scenario9() {
    const req = async () => {
        const resp = await fetch("http://localhost:8080/9")
        if (resp.status >= 300) {
            return Promise.reject(new Error("Not a successful response"))
        }
        else {
            return { now: performance.now(), text: await resp.text() }
        }
    }

    const reqs = Array.from(new Array(10), req)
    const responses = await Promise.allSettled(reqs)
    const successes = responses.filter((result) => result.status === "fulfilled")
    const ordered = successes.sort((a, b) => a.value.now - b.value.now)
    return ordered.reduce((acc, resp) => acc + resp.value.text, "")
}

console.log(await scenario1())
console.log(await scenario2())
console.log(await scenario3())
console.log(await scenario4())
console.log(await scenario5())
console.log(await scenario6())
console.log(await scenario7())
console.log(await scenario8())
console.log(await scenario9())