from flask import Flask, request, redirect
from twilio.twiml.messaging_response import MessagingResponse
import gspread
from oauth2client.service_account import ServiceAccountCredentials
import pandas as pd
import numpy as np
import datetime
from os import environ

app = Flask(__name__)

def get_db():
    """
    Return connection to google sheets
    """
    scope = ['https://spreadsheets.google.com/feeds']
    creds = ServiceAccountCredentials.from_json_keyfile_name('client_secret.json', scope)
    client = gspread.authorize(creds)
    db = client.open("db").sheet1
    return db

def strip_and_cap(word):
    """
    Database convention is sentence case
    """
    word = word.strip()
    return word[0].upper() + word[1:]

def get_df(db):
    """
    Function to read data from google sheets into pandas
    """
    data = db.get_all_values()
    df = pd.DataFrame(data[1:],columns = data[0]).replace('',np.nan).dropna(how = 'all')
    
    #Format as date to make date arithmetic possible
    df['Date'] = pd.to_datetime(df['Date'])
    
    #Format amount as numeric
    df['Amount'] = df['Amount'].astype('float')
    return df 
    
def get_totals(df):
    """
    Compute total amount spent in last two weeks and month
    Returned when new data is spent
    """
    today = pd.to_datetime('today')
    two_weeks = today - datetime.timedelta(days = 14)
    month = today - datetime.timedelta(days = 30)
    two_week_total = df.ix[df['Date'] >= two_weeks,'Amount'].sum()
    month_total = df.ix[df['Date'] >= month,'Amount'].sum()
    return (two_week_total,month_total)

def summary_message(df):
        two_weeks,month = get_totals(df)
        twoweek_total = "You've spent ${:.2f} in the last two weeks".format(two_weeks)
        month_total = " and ${:.2f} in the last month".format(month)
        return twoweek_total + month_total

@app.route("/",methods=['GET', 'POST'])
def respond():
    db = get_db()
    df = get_df(db)
    resp = MessagingResponse()
    received_message = request.values.get('Body').strip()
    if received_message.lower() == 'explain':
        
        # explaining expected format
        message = "Record a purchase by entering the following text:\n Store,Category,Date,Price,Description"                  
        resp.message(message)
    elif received_message.lower() == 'explain categories':
        
        # explaining categories expected by df
        message = "Possible categories are Dining Out,Fun,Groceries,Transportation,Other"
        resp.message(message)
    elif received_message.lower() == 'spending summary':
        
        # report summary without updating google sheets
        message = summary_message(df)
        resp.message(message)
    else:
        
        # cast to lower to not have to do word[1:] in strip_and_cap
        record = received_message.lower().split(',')
        if len(record) < 5:
            message = "Incomplete purchase record. Make sure you enter all fields"
            resp.message(message)
        else:
            record = [x[0].upper().strip() + x[1:].strip() for x in record]
            db.append_row(record)
            
            # update db to reflect most recent purchase in total
            db = get_db()
            df = get_df(db)
            message = "Purchase successfully recorded\n " + summary_message(df)
            resp.message(message)
    return str(resp)

if __name__ == '__main__':
    port = int(environ.get('PORT', 5000))
    app.run(host='0.0.0.0', port=port, debug=False)
