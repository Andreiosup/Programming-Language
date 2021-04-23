import flow

while True:
    text=input('flow > ')
    result,error=flow.run('<stdin>',text)

    if error: print(error.as_srting())
    elif result : print(result)